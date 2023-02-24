\documentclass[12pt]{article}
\usepackage{graphicx} % Required for inserting images
\usepackage{listings}
\usepackage{fontspec}
\usepackage[dvipsnames]{xcolor}
\usepackage[outputdir=build]{minted} % local build
% \usepackage{minted} % overleaf build
\usepackage[fontsize=16pt]{fontsize}
\usepackage{hyperref}
\usepackage{draftwatermark}
\usepackage{metalogo}


\usepackage{geometry}
\geometry{legalpaper, portrait, margin=2cm}

\SetWatermarkText{\texttt{Madeline Sydney Slaga}}
\SetWatermarkScale{0.4}

\usemintedstyle{emacs}

\setmonofont{VictorMonoNF}
[ Extension = .ttf
, UprightFont = res/*-Regular
, ItalicFont = res/*-Italic
, BoldFont = res/*-SemiBold
, SwashFont = res/*-SemiBold
]

% \newenvironment{code}
%     {\begin{minted}[frame=leftline,fontsize=\small]{haskell}}
%     {\\\\\end{minted}}
\newenvironment{code}{\VerbatimEnvironment\begin{minted}[frame=leftline,fontsize=\footnotesize]{haskell}}{\end{minted}}

\newcommand{\mln}[1]{{\footnotesize\mintinline{haskell}{#1}}}
\newcommand{\mono}[1]{{\footnotesize\texttt{#1}}}

\definecolor{greybg}{rgb}{0.95,0.95,0.95}

\title{\vspace{-2.5cm}AP CSP Create Performance Task – Simple wc Implementation}
\author{Madeline ``My Name Stays on my Work'' Slaga}
\date{February 2023}

\begin{document}

\maketitle
Import all required libraries, all of which are part of the
Haskell standard library. This preamble could be significantly
reduced, but it would only promote boring boilerplate.
\begin{code}
-- File IO
import System.IO

-- Command-line argument handling
import System.Environment (getArgs)
import System.Console.GetOpt

-- Very convenient functions; reduce verbosity
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Text.Printf (printf)

-- Used for counting bytes, as the `Char` type can contain Unicode
-- Import qualified because of conflicting identifiers
import qualified Data.ByteString.Char8 as BS (length, pack)
\end{code}

\section{Command-line Options}
We abstract all possible options inside a \mln{Flag} data type.
This ensures a \mln{[Flag]} list can only hold valid options.
\mln{Flag} Derives \mln{Show}, for ease of debugging, and
\mln{Eq}, to test for appearances in a list.
\begin{code}
data Flag
    = Version
    | CountBytes
    | CountLines
    | CountWords
    deriving (Show, Eq)
\end{code}

Define a constant list, \mln{options}, which holds descriptions
(\mln{OptDescr}) of each valid option: their short names, their
long names (only \mono{--version} has one), whether or not they take an argument, and their
description.
\begin{code}
options :: [OptDescr Flag]
options =
    [ Option ['V'] ["version"] (NoArg Version)
        "Display version number"
    , Option ['c'] [ ] (NoArg CountBytes)
        "The number of bytes in each file is written to stdout"
    , Option ['l'] [ ] (NoArg CountLines) 
        "The number of lines in each file is written to stdout"
    , Option ['w'] [ ] (NoArg CountWords) 
        "The number of words in each file is written to stdout"
    ]
\end{code}


\mln{wcOpts} is a function which takes our list of arguments,
and returns and IO action containing a list of set flags, and
a list of non-options. In \mono{wc}'s case, these are the
file paths whose lines, bytes, and words will be counted.

\mln{getOpt} does all of the magic here. It takes an order, \mln{Permute}, meaning arguments and options can be
interspersed in any fashion; a list of option descriptions,
which we defined in the previous block; and our list of raw
arguments. \mln{getOpt} returns a list of recognised flags,
a list of non-options, and a list of error messages. in the
\mln{case ... of} expression, if we don't match exactly the
flags, non-arguments, and an empty error list, we {\it{do}}
have errors, so we return an \mln{ioError} displaying the
errors and a usage message.
\begin{code}
wcOpts :: [String] -> IO ([Flag], [String])
wcOpts argv =
    case getOpt Permute options argv of
        (o,n,[ ] ) ->
            return (o,n)
        (_,_,errs) ->
            ioError $ userError $ errorMsg errs

    where errorMsg e = concat e ++ usageInfo header options
          header = "Usage: wc [-clw] [FILES...]"
\end{code}

\section{Counting}

These are our core functions, and they are extremely simple.
They all take a string, and return an integer. \mln{countLines}
and \mln{countWords} appear the simplest, heavily relying on
Prelude functions. \mln{lines} breaks a string into a list of
its lines, and \mln{words} acts similarly, but with words. The
length of those returned lists are their respective count.

\mln{countBytes} is slightly more complicated, unpacking the string
into a \mln{ByteString} type, essentially a list of octets.
Again, all we need to do is to take the length of this list
(but this time we use the special byte-string length function).
\footnote{The `\mln{.}' operator represents function 
composition. \mln{h = (f.g)} is equivalent to $h = (f \circ g)$}
\begin{code}
countBytes :: String -> Int
countBytes = (BS.length . BS.pack)

countLines :: String -> Int
countLines = length.lines

countWords :: String -> Int
countWords = length.words
\end{code}

Here we have a wrapper for our core functions. It takes a handle for
an open file, and returns a triple containing the number of
lines, words, and bytes, in that order. By virtue of Haskell's
lazy evaluation, this works similar to a stream despite appearing
as if it reads ``read all of \mln{fp} into \mln{content}; count
up the various measures; return the counts.'' In Haskell, the
contents of \mln{fp} are `bound' to \mln{content} in a
mathematical sense, and evaluation is only done when absolutely
required: inside the \mln{countX} functions.
\footnote{Possible optimisation: we traverse \mln{content} three times, instead of a single pass, counting the three measures in parallel.}
\begin{code}
wc :: Handle -> IO (Int, Int, Int)
wc fp = do
    content <- hGetContents fp
    return (countLines content, countWords content, countBytes content)
\end{code}

\section{Output}

This function is a bit of an eyesore. It is used to create the
appropriate output format string for the given options. If no
specific flags are set, they should all be enabled. If specific
flags {\it{are}} set, enable only those specified. Disabled
counts are simply returned as empty strings, while enabled ones
are given as their formatted count, ready for output.
\footnote{Possible optimisation: we use \mln{elem} three times here. This means we traverse the list three times. This could be replaced with a single-pass and a \mln{case ... of}, but wasn't
for clarity.}
\begin{code}
applyOptions :: [Flag] -> (Int, Int, Int) -> (String, String, String)
applyOptions flags (l,w,b) =
    if null flags then
        (lf, wf, bf)
    else
        ( if CountLines `elem` flags then lf else ""
        , if CountWords `elem` flags then wf else ""
        , if CountBytes `elem` flags then bf else ""
        )
    where lf = printf "%8d " l
          wf = printf "%7d " w
          bf = printf "%7d " b
\end{code}

We now build upon our previously-defined function to create our
final output string. This takes our flags, counts, and either
\mln{Just} a string representing the file path, or \mln{Nothing}
in the case of reading from \mono{stdin}.

The first three `\mono{\%s}' sequences in the format string are
substituted with the respective count or an empty string, if
disabled by \mln{applyOptions}. The final `\mono{\%s}' is for
the file path, if it exists.
\begin{code}
wcString :: [Flag] -> (Int, Int, Int) -> Maybe String -> String
wcString flags (l,w,b) path = printf "%s%s%s%s" lf wf bf pathIfFile
    where (lf,wf,bf) = applyOptions flags (l,w,b)
          pathIfFile = fromMaybe "" path
\end{code}


The moment where I finally got to use the elusive \mono{>>=}
operator in a serious context! Here we open a file with
\mln{withFile}, read and take count with \mln{wc}, bind the
result to a lambda expression, which creates our output string
(\mln{wcString}), and finally prints it to \mono{stdout}
(\mln{putStrLn}).
\begin{code}
printWc :: [Flag] -> FilePath -> IO ()
printWc flags path = withFile path ReadMode (\fp ->
        wc fp >>= (\counts ->
            putStrLn $ wcString flags counts $ Just path))
\end{code}

Finally, we can put everything together and define our program's
entry point.

Line 1: We take our program's arguments (\mln{getArgs}), bind
them to (\mln{wcOpts})'s first argument, and bind the parsed flags and non-options to their respective names.

We then check if the list of non-options (files paths) is empty.
If so, we count the requested measures as they appear in
\mono{stdin}. If the list isn't empty (meaning we do have files),
we apply \mln{printWc} to each one. Execution terminates, and the
expected output has been given.
\begin{code}
main :: IO ()
main = do
    (flags, files) <- (getArgs >>= wcOpts)

    if Version `elem` flags then
        putStrLn "wc (pseudo-BSD wc, GNU incompatible) 1.0.0-i'm-not-updating-this"
    else
        if null files then
            wc stdin >>= \counts -> putStrLn $ wcString flags counts Nothing
        else
            forM_ files (printWc flags)
\end{code}

\section{Conclusion}

Fuck the Collegeboard. You lot of evil, slimy, cunts.

\section{Acknowledgements}

\begin{itemize}

    \item My reference implementation, \href{https://www.unix.com/man-page/osx/1/wc/}{\underline{BSD wc(1)}}
    
    \item \href{https://wiki.haskell.org/Merely_monadic}{\underline{The Haskell Wiki – Merely Monadic}}

    \item \href{https://hackage.haskell.org/package/base-4.17.0.0/docs/System-Console-GetOpt.html#g:3}{\underline{Hackage \mono{base} Docs – GetOpt Example}}

    \item \href{https://latex-tutorial.com/tutorials/hyperlinks/}{\underline{How to Add Hyperlinks in LaTeX (lol)}}

	\item The font used for all monospace text in this document, \href{https://rubjo.github.io/victor-mono/}{\underline{Victor Mono}}

    \item may many gems and jewels be awarded to the developers of NeoVim, Overleaf, \XeTeX, \TeX live, GHC, GNU Make, and Zathura.

    \item This document's source code is available \href{https://github.com/msydneyslaga/wc}{\underline{here}}
\end{itemize}

\end{document}


