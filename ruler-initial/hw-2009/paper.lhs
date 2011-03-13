\documentclass[preprint,natbib]{sigplanconf}

%include lhs2TeX.fmt
%include polycode.fmt

\usepackage{mathptmx}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{amsthm}
\usepackage{stmaryrd}
\usepackage{txfonts}
\usepackage{float}
\usepackage{mathpartir}
\usepackage{xcolor}
\usepackage{natbib}
\usepackage{comicsans}

\newcommand\Rule{r}
\newcommand\Rules{\Rule^*}
\newcommand\Statement{c}
\newcommand\Statements{\Statement^*}
\newcommand\Bindings{\Delta}
\newcommand\Env{\Gamma}
\newcommand\Scheme{\Sigma}
\newcommand\Schemes{\Scheme^*}
\newcommand\SchemeName{s}
\newcommand\Type{\tau}
\newcommand\Idents[1]{{#1}^{\!*}}
\newcommand\In{\mbox{in}}
\newcommand\Out{\mbox{out}}
\newcommand\Return{\mathsf{return}}
\newcommand\Abort{\mathsf{error}}
\newcommand\Fail{\mathsf{fail}}
\newcommand\Unify{\mathsf{unify}}
\newcommand\Do{\mathsf{do}}
\newcommand\Haskell{H_{\!\lambda}}
\newcommand\Execution{\mathsf{exec}}
\newcommand\Defer{\mathsf{defer}}
\newcommand\Fixate{\mathsf{fixate}}
\newcommand\Equal{\mathsf{equal}}
\newcommand\Fixpoint{\mathsf{fixpoint}}
\newcommand\Commit{\mathsf{commit}}
\newcommand\Onebyone{\mathsf{onebyone}}
\newcommand\Force{\mathsf{force}}
\newcommand\Let{\mathsf{let}}
\newcommand\Update{\mathbf{update}}
\newcommand\typeofinput{\lhd}
\newcommand\typeofoutput{\rhd}
\newcommand\Heap{\mathcal{H}}
\newcommand\Prop{\rho}
\newcommand\Props{\Prop^{*}}
\newcommand\Substitution{\theta}
\newcommand\Scope{\zeta}
\newcommand\Deferred{\mathbf{deferred}}
\newcommand\Deriv{\pi}
\newcommand\Derivs{\pi^*}
\newcommand\DerivsEnv{\Pi}
\newcommand\RuleIdent{\iota}
\newcommand\sembrack[1]{\llbracket #1 \rrbracket}
\newcommand\attrsep{\;\textcolor{blue}{;}\;}
\newcommand\derivref[1]{!#1}
\newcommand\transition[2]{\;\;\textcolor{blue}{\rightarrow}^{#1}_{#2}\;\;}
\newcommand\erasure[1]{\lfloor #1 \rfloor}
\newcommand\lightbrack[1]{\;\textcolor{gray}{(} #1 \textcolor{gray}{)}\;}
\newcommand\tybox[1]{\fbox{$#1$}}
\newcommand\removeboxes[1]{\lfloor #1 \rfloor}
\newcommand\boxexpr[2]{\tybox{#1}_{\: #2}}

\newcommand\RulerCore{\ensuremath{\mathsf{Ruler Core}}}
\newcommand\RulerBase{\ensuremath{\mathsf{Ruler Base}}}

\newtheorem{thm}{Theorem}[section]
\newtheorem{lem}[thm]{Lemma}

\floatstyle{boxed}
\restylefloat{figure}
\setlength{\fboxsep}{1.4pt}

\bibpunct();A{}

%format Substitution = "\Substitution"
%format Scope        = "\Scope"
%format |=>          = "\Mapsto"
%format Var          = "v"
%format notelem      = "\not\in"
%format alpha        = "\alpha"
%format forall       = "\forall\!"
%format beta         = "\beta"
%format v1           = "v_1"
%format v2           = "v_2"
%format semv         = "\sembrack{v}"
%format semvf        = "\sembrack{v}_F"
%format semv1        = "\sembrack{v_1}"
%format semv2        = "\sembrack{v_2}"
%format Gamma        = "\Gamma"
%format alphas       = "\overline{\alpha}"
%format tau          = "\tau"
%format input        = "\mathbf{input}"
%format output       = "\mathbf{output}"
%format pattern      = "\mathbf{pattern}"
%format dot          = ".\;"
%format ty1          = "\tau_1"
%format ty2          = "\tau_2"
%format striproot    = "\removeboxes{\tau}"
%format tau          = "\tau"
%format ^^^          = " \hspace{1em}"
%format boxexpr t b  = "\boxexpr{" t "}{" b "}"
%format tybox t      = "\tybox{" t "}"
%format return       = "\mathbf{return}"
%format refine       = "\mathbf{refine}"
%format sIn          = "s_{\mbox{in}}"
%format sIn'         = "s'_{\mbox{in}}"
%format sOut         = "s_{\mbox{out}}"
%format t1           = "t_1"
%format t2           = "t_2"
%format t3           = "t_3"
%format t4           = "t_4"
%format w1           = "w_1"
%format w2           = "w_2"
%format w3           = "w_3"
%format w4           = "w_4"
%format tIn          = "t_{\mbox{in}}"
%format tOut         = "t_{\mbox{out}}"
%format unif         = "\mathbf{unif}"
%format unifOne      = "\mathbf{unifOne}"
%format defer        = "\mathbf{defer}"
%format commit       = "\mathbf{commit}"
%format >>           = "\gg"
%format ^            = "\;\;\;"
%format tauf         = "\tau_f"
%format taua         = "\tau_a"
%format taur         = "\tau_r"
%format gam          = "\Gamma"
%format <+>          = "\oplus"
%format update       = "\mathbf{update}"
%format fail         = "\mathbf{fail}"
%format abort        = "\mathbf{abort}"

\begin{document}

\conferenceinfo{HW '09}{September 3, 2009, Edinburgh.} 
\copyrightyear{2009} 
\copyrightdata{[to be supplied]}

%% \titlebanner{banner above paper title}        % These are ignored unless
%% \preprintfooter{short description of paper}   % 'preprint' option specified.

\title{Controlling Non-Determinism in Type Rules using First-Class Guessing}

\authorinfo{Arie Middelkoop \and Atze Dijkstra \and S.~Doaitse Swierstra}
           {Universiteit Utrecht}
           {\{ariem, atze, doaitse\}@@cs.uu.nl}
\authorinfo{Luc\'{\i}lia Camar\~{a}o de Figueiredo}
           {Universidade Federal de Ouro Preto}
           {lucilia@@dcc.ufmg.br}

\maketitle

\begin{abstract} 
Given a type system written as a collection of type rules, we
investigate the automatic derivation of inference algorithms
from these rules.
A minor challenge are the side effects of a rule, which need to
be expressed algorithmically. A major challenge are
non-deterministic aspects of rules that cannot be directly mapped
to an algorithm.

We present Ruler, a language for type inferencers, to meet these
challenges. An inferencer is written as a collection of rules
with side conditions explicitly expressed in Haskell, and with
annotations for the scheduling of the rules.

This paper includes an extensive case study of an inferencer
for the ``First Class Polymorphism for Haskell'' type
system~\cite{DBLP:conf/icfp/VytiniotisWJ08}.
\end{abstract}

% \category{CR-number}{subcategory}{third-level}

% \terms
% term1, term2

% \keywords
% keyword1, keyword2

\section{Introduction}

A type system is ``a tractable syntactic method for proving the absence
of certain program behaviors by classifying phrases according to the
kinds of value they compute''~\cite{509043}. Given a type
system, it is often not immediately clear whether there exists an algorithm that can
automatically infer a valid type for a (type correct) program. More
specifically, if the type system has principal types, is there an
algorithm that can infer the most general type of each expression? 

Most type systems have a declarative specification in the form of a collection of type rules. How to effectively and efficiently use these rules for building type correctness proofs is a separate issue, and having a systematic way in building such type inferencers from such a collection of rules is still an open issue.
The benefits of having such a method are:

\begin{description}
\item[Consistency.] A strong coupling between formal description and implementation makes it easier to show how certain
  properties proved for the type system carry over to the inferencer.
\item[Rapid prototyping.] Experimenting with type systems leads to a deeper understanding. However,
  language developers are currently discouraged to do so as it is cumbersome to write inferencers from scratch. 
  A framework will relieve programmers from this burden and hence support rapid prototyping.
\item[Abstraction.] Interacting language features obscure and complicate language semantics. In many inference
  algorithms, the unification procedure makes the essential decisions about non-deterministic aspects. This requires context-information to
  be carried to and into the place where unification is performed, and complicates the inferencer. 
  Instead, we would like to be able to deal
  with the decision making process at the places where the non-deterministic aspects occur in the type rules.
\item[Documentation.] In comparison with the type rules, the type inference algorithms are often not completely documented and explained. Often they are specified by a concrete (and sometimes obscure) implementation.
\end{description}

This paper shows that it is possible to semi-automatically obtain inference
algorithms from type rules. We do not get them entirely for free. A
major problem is that type rules generally contain non-deterministic
aspects. For example, more than a single rule may be applicable at the
same time. Even when the rules are syntax directed, they may state
demands in the form of side conditions about a type (or other value),
of which concrete information is not easily available from the context.

A solution to this challenge are {\it annotations} which control the
scheduling of the resolution of non-deterministic aspects by
manipulating guesses. A {\it guess} is an opaque value representing a
derivation that has not been constructed yet.  It also serves as a place
holder for a concrete value. We can pass such a guesses around, observe
them, impose requirements on them. When a sufficient number of requirements
have been accumulated, the actual value of the guess is revealed and we can
attempt to construct the derivation.

Therefore, we contribute the following:
\begin{itemize} 
\item We present a typed domain specific language for type inferencers called Ruler. One of its distinguishing features is
  the possibility to provide annotation for type rules. Also, side expressions are expressed using conventional Haskell code.
\item We give examples of increasing complexity of inferencers for type systems with non-deterministic aspects,
  and show how manipulating guesses leads to their resolution (Section~\ref{sect:examples}). We demonstrate the power of these annotations by providing an inferencer for the type system of FPH \cite{DBLP:conf/icfp/VytiniotisWJ08} that is directly based on FPH's collection of declarative type rules (Section~\ref{sect:example-fph}).
\item We formalize the notation (Section~\ref{sect:syntax}), the operational semantics (Section~\ref{sect:operational-semantics}) and the static semantics (Appendix~\ref{sect:static-semantics}) of Ruler.
\item We discuss the rationale of our design compared to prior work on the construction of type inferencers (Section~\ref{sect:related-work}).
\item We have proof-of-concept Haskell-based implementation for a meta-typed front-end in which inferencer rules with custom syntax can be encoded.
  Furthermore we provide an executable version of the operational semantics which interprets the inference rules and produces a derivation
  in terms of the original type rules for all expressions it manages to type.
\end{itemize}

The reason that we have chosen Haskell as the target language for our generated inferencers are:
\begin{itemize}
\item We can use the expressiveness of Haskell for writing the semantics
  of side conditions in type rules.
\item There are many libraries available for Haskell that provide efficient data structures and external constraint solvers.
\item We can integrate the inferencer with other Haskell projects, in particular the Utrecht
  Haskell Compiler~\cite{DBLP:conf/ifl/DijkstraFS07}. We have compiler technology readily
  available (parsers, tree-walk generators, pretty printers, etc.) to facilitate
  rapid prototyping.
\end{itemize}


\section{Examples}
\label{sect:examples}

  In this section we show how to use Ruler in describing a series of type inferencers of increasing complexity.
  We took the examples such
  that each example builds on the previous one. We start from an
  inferencer for the explicitly typed lambda calculus in
  Section~\ref{sect:example-explicit}. Admittedly, the inferencer
  in this case does only type checking, but we use it to informally
  introduce the Ruler inferencer language (formally in
  Section~\ref{sect:syntax}) and informally describe its evaluation
  model (formally in Section~\ref{sect:operational-semantics}). Then,
  in Section~\ref{sect:example-poly}, we move on to an inferencer for
  implicitly typed System F, in which several cases of
  non-determinism arise. Finally, we show the inferencer for FPH in
  Section~\ref{sect:example-fph}, which demonstrates the expressive power of the
  annotations.

  For each example we show the type rules and the actual inferencer
  code. As they have a tight resemblance, be warned not to confuse the
  two!

  \begin{figure}[htp]
    Syntax:
    \begin{displaymath}
    \begin{array}{lll}
    e     & = & x  \;\mid\;  |f a| \;\mid\;  \lambda (x :: \tau) . \: e \;\mid\; |let x = e in b|  \;\mid\;  \mathbf{fix} \: f \\
    \tau  & = & \alpha \;\mid\; \tau_1 \rightarrow \tau_2
    \end{array}
    \end{displaymath}
    Rules:
    \begin{mathpar}
        \inferrule*[right=var]
      { (x, \tau) \in \Gamma }
      { \Gamma \vdash x : \tau }

    \inferrule*[right=app]
      { \Gamma \vdash f : \tau_f \\
        \Gamma \vdash a : \tau_a \\\\
        \tau_f \equiv \tau_a \rightarrow \tau_r \\
      }
      { \Gamma \vdash f \: a : \tau_r }

    \inferrule*[right=lam.expl]
      { x :: \tau_x, \Gamma \vdash e : \tau }
      { \Gamma \vdash \lambda (x :: \tau_x) . e : \tau_x \rightarrow \tau }

    \inferrule*[right=let]
      { \Gamma \vdash e : \tau_x \\\\
        x :: \tau_x, \Gamma \vdash b : \tau \\
      }
      { \Gamma \vdash |let x = e in b| : \tau }

    \inferrule*[right=fix]
      { \Gamma \vdash f : \tau_f \\
        \tau_f \equiv \tau \rightarrow \tau \\
      }
      { \Gamma \vdash \mathbf{fix} \; f : \tau }
    \end{mathpar}
    \caption{Type system for explicitly typed lambda calculus.}
    \label{fig:example-ts-explicit}
    \end{figure}

  \subsection{Explicitly Typed Lambda Calculus}
  \label{sect:example-explicit} 

Figure ~\ref{fig:example-ts-explicit} gives the type system for the
explicitly typed lambda
calculus~\cite{DBLP:journals/jsyml/Church40,509043}. The rules define
a {\it relation} between an environment $\Gamma$, expression $e$, and
type $\tau$. 

For the inferencer, this corresponds to a {\it function} (we call it a
{\it scheme}) that takes an environment $\Gamma$ and expression $e$ as
inputs and produces a valid type $\tau$, if there exist such a type
according to the rules. The Ruler code of the inferencer of this type
system is given in Figure~\ref{fig:example-explicit}.  We discuss each
part further below.

  \begin{figure}[htp]
  Scheme declarations:
  \begin{displaymath}
    \begin{array}{lll}
    \mathsf{tc:}  &                              & \lightbrack{\Gamma \typeofinput \mathsf{Map}\:\mathsf{String}\:\mathsf{Ty}} \vdash \lightbrack{e \typeofinput \mathsf{Expr}} \;:\; \lightbrack{\Type \typeofoutput_{\mbox{d}} \mathsf{Ty}} \\
    \mathsf{fr:}  & \forall \alpha .\;\;         & \lightbrack{v \typeofoutput_{\mbox{d}} \alpha} \; \mbox{fresh} \\
    \mathsf{lk:}  & \forall \alpha \beta .\;\;   & (\lightbrack{k \typeofinput \alpha}, \lightbrack{v \typeofoutput \beta}) \in \lightbrack{\Gamma \typeofinput \mathsf{Map}\;\alpha\;\beta} \\
    \end{array}
  \end{displaymath}
  Inferencer rules:
  \begin{mathpar}
    \inferrule*[right=var]
      { (x, \tau) \in \Gamma }
      { \Gamma \vdash x : \tau }

    \inferrule*[right=lookup]
      { |v <- lookup x| \; \Gamma }
      { (x, v) \in \Gamma }

    \inferrule*[right=app]
      { \Gamma \vdash f : \tau_f \\
        \Gamma \vdash a : \tau_a \\\\
        \tau_r \; \mbox{fresh} \\
        \tau_f \equiv \tau_a \rightarrow \tau_r \\
      }
      { \Gamma \vdash f \: a : \tau_r }
    
    \inferrule*[right=fresh]
      { \Defer_v \; [\: \emptyset \:] }
      { v \; \mbox{fresh} }
    
    \inferrule*[right=lam.expl]
      { x :: \tau_x, \Gamma \vdash e : \tau }
      { \Gamma \vdash \lambda (x :: \tau_x) . e : \tau_x \rightarrow \tau } \\
    
    \inferrule*[right=let]
      { \Gamma \vdash e : \tau_x \\\\
        x :: \tau_x, \Gamma \vdash b : \tau \\
      }
      { \Gamma \vdash |let x = e in b| : \tau }
    
    \inferrule*[right=fix]
      { \tau \; \mbox{fresh} \\\\
        \Gamma \vdash f : \tau_f \\
        \tau_f \equiv \tau \rightarrow \tau \\
      }
      { \Gamma \vdash \mathbf{fix} \; f : \tau }
    \end{mathpar}
    Syntax and semantics:
    \begin{code}
      data Ty  =  TGuess  GuessVar    ^  data Expr  =  EVar String
               |  TConst  GuessVar                  |  EApp Expr Expr
               |  TArr    Ty   Ty                   |  ...

      instance Container Ty where
        appSubst rec (TArr f a)  = rec f >> rec a
        deferVars (TConst _)     = empty
        deferVars (TGuess v)     = single v
        deferVars (TArr a r)     = deferVars a `union` deferVars r

      instance Unifyable Ty where
        unify rec  (TArr f a)  (TArr g b)  = rec f g >> rec a b
        unify _    _           _           = fail "type error"

      instance Deferrable Ty where
        mkDeferValue                = TGuess
        mkFixedValue                = TConst
        matchDeferValue (TGuess v)  = Just v
        matchFixedValue (TConst v)  = Just v

      pattern Map String Ty  where  x :: tau, Gamma  input   insert x tau Gamma
      pattern Expr           where  \x dot e         output  ELam x e
      -- other patterns omitted.
    \end{code}
    \caption{Inferencer for explicitly typed lambda calculus.}
    \label{fig:example-explicit}
    \end{figure}

\paragraph{Scheme declarations.}  To obtain an inferencer in Haskell,
we actually want a Haskell function |tc| with the type: |Map String Ty
-> Expr -> I Ty| where |I| is some monad encapsulating failure and
state. Thus, concerning the meta variables, we need to know whether
they serve as inputs or outputs, and what their meta type is. This is
information is given in the scheme declaration. It defines the name of
the function (i.e. |tc|), the syntax of the function call in the
inferencer rules (scheme instantiation), the names and types of the
meta variables, whether a meta variable is an input ($\typeofinput$)
or an output ($\typeofoutput$), and a optional property $\mbox{d}$ or
$\mbox{u}$ of a meta variable. In this case, the $\mbox{d}$-property
requires that we supply an instance of |Deferrable| for the Haskell
type |Ty|, and allows us to use the $\Defer$ statement on types (to be
explained later).

\paragraph{Inferencer rules.}  The inferencer rules provide the actual
definition of the scheme. They consist of an ordered sequence of {\it
statements}, related to the {\it premises} of the type rules, and a
concluding statement. Such a statement can be:

\begin{itemize}
    \item A scheme invocation, i.e. $(x, \tau) \in \Gamma$, which executes the corresponding function with the given parameters when evaluated.
    \item Haskell code in the |I| monad. This code is used to express side-conditions of type rules as statements
          in the inferencer rules. For example, the type system in Figure~\ref{fig:example-explicit} implicitly mentions a
          lookup-relation in the $\RefTirName{VAR}$ rule. This is explicitly defined in our inferencer code by means of some Haskell code
          in the $\RefTirName{LOOKUP}$ rule.
    \item An equality statement, i.e. $\tau_f \equiv \tau_a \rightarrow \tau_r$, stating that its two inputs will be the same after type
          inference has finished.
    \item Non-determinism annotations, such as $\Defer$ (explained later).
\end{itemize}

    The rules represent the actual definition of cases for functions |tc|, |lk|, and |fr|. For example, the $\RefTirName{APP}$, $\RefTirName{LOOKUP}$,
    and $\RefTirName{FRESH}$ inferencer rules are projected to concrete Haskell code as follows:
    \begin{code}
      tc_app gam e                        ^^^  lk_lookup x gam
        = do  let (EApp f a) = e          ^^^    = do  v <- lookup x gam
              tauf  <- tc gam f           ^^^          return v
              taua  <- tc gam a           ^^^  lk = lk_lookup
              taur  <- fr                 ^^^  fr_fresh = do  (v, ()) <- defer f
              unif tauf (TArr taua taur)  ^^^                 return v
              return taur                 ^^^    where f v' = return ()
      tc = tc_var <+> tc_app <+> ...      ^^^  fr = fr_fresh
    \end{code}

    The equivalence statement gets translated to a monadic expression |unif| which is an API function provided by Ruler.

    The statements are executed in the order of appearance.
    Each statement may fail, causing the entire rule to fail. Rules that fail due to pattern matches or equality statements at the very beginning
    of the statement sequence allow other applicable rules to be applied (offering a limited form of backtracking). Otherwise, the failure
    is turned into an abort of the entire inference, with a type error as result.

    \paragraph{Non-determinism.}

    A major recurring problem is that not all relations are
    functions. Sometimes a meta variable is required to be both an
    input and an output.  For example, in the inferencer rule
    $\RefTirName{APP}$, the value $\tau_r$ needs to be produced before
    it can be passed to the equality statement. It is also an output
    of the rule, not an input. This means that there is no indication
    how to obtain it.  Therefore, we conceptually {\it guess}
    the value of $\tau_r$. This value is kept hidden behind an opaque
    guess-value, and is only revealed when we actually discover what
    the value must be. The |fr|-scheme gives a function that
    produces these values.

    Ruler accomplishes this as follows. The rule $\RefTirName{FRESH}$
    has a $\Defer$-statement, which is a non-determinism
    annotation. It is parametrized with a list of statement
    sequences, and produces a guess $v$. The statement sequences are
    not executed immediately, but a closure is created for them which
    is triggered once we discover concrete information about guess
    $v$. At that point, one of the statement sequences is required to
    execute successfully with $v$ as an input and the current
    knowledge about guesses. The $\Defer$-statement in
    $\RefTirName{FRESH}$ has only one statement sequence, the empty
    sequence, which always succeeds. In later examples we have
    non-trivial sequences of statements that allow us to defer and
    control decision making.

    So, a guess needs to get produced for $v$. Ruler requires
    help in the form of a |Deferrable| instance on the type of $v$ to
    construct this guess. Operationally, defer produces an opaque
    guess variable, which is wrapped into the domain of $v$ by means
    of |mkDeferValue|.  This guess is thus first class, and can be
    passed around and end up in other data structures. Ruler maintains
    information about these guess variables, such as the closures
    produced by $\Defer$. When a concrete value is discovered about a
    guess, all occurrences of this guess are replaced with this
    concrete value (thus revealing the guess). Again, Ruler requires
    help by means of a |Container| instance in order to deal with
    values holding guesses. Furthermore, the inferencer rules may
    check if certain values are still opaque variables and act on
    that. This is also something we exploit later.

    Concrete values for a guess are discovered by executing equality
    statements. When comparing the two input values, if one value
    contains a guess and the other a concrete value, then we {\it
    commit} that concrete value to the guess. This leads to the
    execution of the deferrable statements. We call this commit because it
    is an irreversible action: a guess can be opaque for a while, a
    commit conceptually only uncovers it. If both values are guesses,
    the guesses are merged. In case both values are concrete values,
    Ruler requires help in the form of a |Unifyable| instance, of
    which |unify| is required to traverse one level through the values
    and check that their heads are the same. Another requirement on
    guesses is that the value committed to a guess may not contain the
    guess itself (the infamous {\it occur check}). Therefore, the
    function |deferVars| needs to be defined to tell Ruler which
    guesses are contained in a value.

    \paragraph{Data semantics.}  The last part of the Ruler code
    consists of a definition of the data structures involved. One may
    also define custom syntax to be used in the rules, for which
    translations to either Haskell patterns (for inputs) or Haskell
    expressions (for outputs) need to be given. This custom syntax may
    be ambiguous as long as it can be resolved based on the meta types
    of the meta variables.  Finally, we remark that these instances for
    data types are likely to be automatically generated from the
    structure of the data types, or readily available in a library
    with support code.\\

    Before we continue, consider the addition of an extra rule to the inferencer:
    \begin{mathpar}
      \inferrule*[right=lam.impl]
        { \tau_x \; \mbox{fresh} \\
          x :: \tau_x, \Gamma \vdash e : \tau \\
        }
        { \Gamma \vdash \lambda x . e : \tau_x \rightarrow \tau }
    \end{mathpar}

    With this addition, we obtain an inferencer for the simply typed
    lambda calculus.  However, with this rule, there can be unresolved
    guesses remaining after we finish inferencing. For example,
    consider inferring the type of the expression |\x dot x|. We
    obtain |tau -> tau|, where |tau| is a guess.  However, at the end
    of the inference, Ruler forces all remaining guesses to get
    evaluated. Those guesses that remain are mapped to a {\it fixed}
    value. A fixed value is an opaque value like a guess (created with
    |mkFixedValue|), except that it is only equal to itself and cannot
    be committed on.

    %% Annette: I would leave this paragraph out
%    We conclude this example by answering the question of why not
%    writing an embedded domain specific language for Haskell. We have
%    many reasons: the custom syntax, automatic insertion of fresh and
%    |\equiv| based on meta variable direction, compilation to
%    different backends (attribute grammars of the UHC for example),
%    and analysis of pattern matches to discover which rules are syntax
%    directed and which are overlapping.

  \subsection{Implicitly Typed System F}
  \label{sect:example-poly}

    We give a sound but incomplete inferencer for implicitly typed System F~\cite{721503}, using a relaxation of Milner's
    algorithm~\cite{DBLP:journals/jcss/Milner78} and exploiting type annotations to deal with higher-ranked types. Compilers
    such as GHC and UHC utilize inference algorithms based on this type system, which makes it an interesting case study.
    The inferencer algorithm described here is clearly inferior versus other algorithms in terms of completeness and
    predictability, but is powerful and simple enough to serve as a basis for the inference algorithm of the next section.

    \begin{figure}[htp]
    Extra syntax:
    \begin{displaymath}
      \tau  =  \ldots  \;\mid\; \forall \overline{\alpha} .\: \tau
    \end{displaymath}
    Extra rules:
    \begin{mathpar}
      \inferrule*[right=inst]
        { \Gamma \vdash e : \forall \overline{\alpha} . \:\tau_2 }
        { \Gamma \vdash e : [\overline{\alpha \coloneqq \tau_1}] \: \tau_2 }
      
      \inferrule*[right=gen]
        { \Gamma \vdash e : \tau_1 \\
          \overline{\alpha} \not\in |ftv| \: \Gamma \\
        }
        { \Gamma \vdash e : \forall \overline{\alpha} . \: \tau_1 }
    \end{mathpar}
    \caption{Type system of implicitly typed System F.}
    \label{fig:example-ts-poly}
    \end{figure}

    Implicitly typed System F (or polymorphic lambda calculus) extends the simply typed lambda calculus with two rules, and
    a more expressive type language (Figure~\ref{fig:example-poly}). This change adds a lot of expressive
    power. Consider the following expressions (assuming for the moment that we have |Int|s in the type language):
    \begin{code}
    f  = \(k :: (Int -> Int) -> Int) dot                         k (\x dot x)
    g  = \(k :: forall alpha dot (alpha -> alpha) -> Int) dot ^  k (\x dot x)
    \end{code}
    The definition of |f| can be typed within the simply typed lambda calculus, but |g| cannot. In |g|'s case, the $\RefTirName{GEN}$
    rule is needed after typing |\x dot x|.

    There is no simple way to translate these rules to the type
    inferencer rules. The problem lies in the decision when to apply
    these rules, because this is not specified by the syntax. They
    could be applied any time, even an arbitrary number of
    times. However, we choose to only apply instantiation once
    directly after the $\RefTirName{VAR}$ rule, and generalization
    once for each let-binding, and once for the argument of each
    application. Figure~\ref{fig:example-poly} lists the inferencer
    rules. We partitioned the rules such that they belong
    either to scheme $\vdash$, $\vdash_x$, $\vdash_g$, or $\vdash_l$,
    and adapted the recursive invocations accordingly. This solves the
    problem of when to apply the rules.  Also note that we have two
    versions of the $\RefTirName{GEN}$ rule, one for the let-binding
    ($\RefTirName{GEN.LET}$), and one for the argument of an
    application ($\RefTirName{GEN.LAZY}$).

    \begin{figure}[htp]
    Inferencer rules:
    \begin{mathpar}
      \inferrule*[right=var]
        { (x, \tau) \in \Gamma }
        { \Gamma \vdash_x x : \tau }

      \inferrule*[right=inst.v]
        { \Gamma \vdash_x x : \forall \overline{\alpha} . \:\tau_2 \\
          \overline{\tau_1} \; \mbox{fresh} \\
        }
        { \Gamma \vdash x : [\overline{\alpha \coloneqq \tau_1}] \: \tau_2 }

      \inferrule*[right=gen.lazy]
        { \Gamma \vdash e : \tau_1 \\
          \Defer_{\tau_2} \: [[\; \Let \: (\forall \overline{\alpha} . \tau'_2) = \tau_2, \; \tau_1 \equiv \tau'_2, \; \overline{\alpha} \not\in |ftv| \: \Gamma \;]]
        }
        { \Gamma \vdash_g e : \tau_2 }

      \inferrule*[right=app]
        { \tau_r \; \mbox{fresh} \\\\
          \Gamma \vdash f : \tau_f \\
          \Gamma \vdash_g a : \tau_a \\\\
          \tau_f \equiv \tau_a \rightarrow \tau_r \\
        }
        { \Gamma \vdash f \: a : \tau_r }

      \inferrule*[right=gen.let]
        { \Fixate_{\tau} \: [\; \Gamma \vdash e : \tau \;] \\\\
          \Let \: \overline{\alpha} = |ftv| \: \tau |- ftv|\: \Gamma \\
        }
        { \Gamma \vdash_l e : \forall \overline{\alpha} . \: \tau}

      \inferrule*[right=let]
        { \Gamma \vdash_l e : \tau_x \\\\
          x :: \tau_x, \Gamma \vdash b : \tau \\
        }
        { \Gamma \vdash |let x = e in b| : \tau }
    \end{mathpar}
    Syntax and semantics:
    \begin{code}
    data Ty = ... ^^^ | ^^^ TAll [GuessVar] Ty
    \end{code}
    \begin{code}
    ftv (TConst v)  = single v   ^^^  ftv (TArr a r)   = ftv a `union` ftv r
    ftv (TGuess _)  = empty           ftv (TAll vs t)  = ftv t `difference` vs
    \end{code}
    \caption{Inferencer for implicitly typed System F.}
    \label{fig:example-poly}
    \end{figure}

    However, this leads us back to the non-determinism problems that
    we encountered before.  The $\RefTirName{INST.V}$ rule requires us
    to choose which bound variables to instantiate, and what type to
    instantiate them to. Similarly, for both $\RefTirName{GEN}$ rules,
    a decision needs to be made what variables to generalize
    over. These are all examples of non-deterministic aspects. We use
    the following tricks to resolve them:

    \begin{itemize} 
    \item Instantiation (rule $\RefTirName{INST.V}$)
    is greedy and instantiates all bound variables that are know at
    the time when instantiation is applied. We use the |fr|-relation
    to guess the types to which they are instantiated.
    \item Generalization of the argument of an application (rule
    $\RefTirName{GEN.LAZY}$) is done on-demand. The result type
    $\tau_2$ is guessed. At some point the head (or more) of $\tau_2$
    is discovered. In case of our example: for |f| we
    discover at some point that $\tau_2$ is |Int -> Int|, and for |g|
    that it is $\forall \alpha . \alpha \rightarrow \alpha$. At that
    moment the deferred statements are triggered.  

    When these statements trigger, the requirement is that enough
    information about the outermost quantifiers of $\tau_2$ is known.
    Furthermore, with the greedy assumption about instantiation,
    assume that $\tau_1$ does not have any outermost quantifiers. With
    this knowledge in mind, consider the $\RefTirName{GEN}$ type rule
    again in Figure~\ref{fig:example-ts-poly}. The type rule tells us
    to take the portion of $\tau_1$ without outermost quantifiers,
    this should then be equal to $\tau_2$. In that case, the variables
    $\overline{\alpha}$ are not allowed to be in the environment. This
    is exactly what the deferred statements of $\RefTirName{GEN.LAZY}$
    establish.
    \item Generalization just before the let-binding is also
    greedy. It generalizes over all unbound variables in the type that
    are not in the environment. However, since a guess can represent
    an arbitrary type, we cannot generalize over them. Therefore, we
    introduce the $\Fixate$-statement. It is parametrized with a
    sequence of statements, and executes those.  The guesses
    which are introduced during the execution and remain are forced to be
    evaluated. Those for which no concrete value is discovered are
    mapped to fixed types (|TConst| values). The order of this forcing
    is undefined. These |TConst| values are real type variables and
    can be generalized over (if free in the environment).
    \end{itemize}

    \begin{figure}[htp]
    \small
    \begin{mathpar}
      \inferrule*
        { q, \tau_x \: \mbox{fresh} \\\\
          \Fixate \left[ \!\!\!\! \mbox{\begin{tabular}{l}
                           $\Defer_{|_|} \: [ \tau_x \equiv |pick| \: q ]$ \\
                           $(x, \tau_x, q), \Gamma \vdash e : \tau$ \\
                         \end{tabular}} \!\!\!\! \right] \\
        }
        {\Gamma \vdash \lambda x . e : \tau_x \rightarrow \tau }

      \inferrule*
        { (x, \tau_1, q) \in \Gamma \\\\
          \Defer_{\tau_2} [ q' \: \mbox{fresh}, \Commit_{(|last| \: q)} \: (\tau_2, q') ] \\\\
          \Defer_{\tau_1'} \: [\; \tau_1' \leq \tau_2  \;] \\
          \tau_1 \equiv \tau_1' \\
        }
        {\Gamma \vdash x : \tau_2 }
    \end{mathpar}
    We defer the instantiation $\leq$ until we have more information about all the types we want
    to instantiate the left-hand side to.
    Queue $q$ is a nested product, where each left component is a type, and each right component
    is a queue. This queue is terminated with a guess. The queue stores all encountered values
    for the type of the lambda parameter. Each time such a value is encountered, it gets appended
    to the queue. When fixating the lambda term, the deferred statement
    executes that traverses the queue and picks out the most general type, and matches it
    with the type of the lambda parameter. This causes all deferred instantiations $\leq$ to execute.
    The  $\leq$ relation is not affected by this complex scheduling.
    \caption{Complex example: queuing all expected types.}
    \label{fig:example-queing}
    \end{figure}

    Many variations on the above rules are possible that result in a
    more complete inference algorithm (although no complete inference
    algorithm exists). For example, making instantiation also happen
    on demand, or queuing up all guesses of the type of lambda
    parameters (see Figure~\ref{fig:example-queing}) before making a
    final choice, thus emulating type propagation
    algorithms~\cite{DBLP:journals/jfp/JonesVWS07, UUCS2006051}.  Such
    algorithms are normally very hard to implement, because with
    conventional approaches the unification algorithm has to deal with
    it all. However, with Ruler, such algorithms can now be described
    easily, and locally at the places in the rules where full
    context-information is available, with only minimal effects on
    modularity.  

    \subsection{Summary} We have seen several examples of
    non-determinism that are problematic when writing an inference
    algorithm. In the end, these problems boil down to deferring
    decisions and controlling the decision process. We have seen and
    will see the following annotations to resolve them:

    \begin{description}
    \item [$\Defer$] introduces a guess with the promise that a sequence of statements will
       be executed when the guess is revealed.
    \item [$\Fixate$] introduces a scope for guesses and forces all guesses introduced in this
       scope to be resolved when leaving the scope.
    \item [$\Commit$] unveils the guess, causing the deferred statements to run.
    \item [$\Force$] is syntactic sugar for a $\Commit$ with a fresh guess, followed by a
      commit with a fixed value if the result was still a guess.
    \end{description}
    The driving force behind propagating the type information that slowly comes available are
    the equality statements ($\equiv$).

  \subsection{Example: FPH}
  \label{sect:example-fph}

    The FPH type system~\cite{DBLP:conf/icfp/VytiniotisWJ08} is a restriction of implicitly typed System F, such that there exist a principle
    type for each binding, and a complete inference algorithm that finds these types. In this section, we give an alternative inference
    algorithm. Compare the FPH's declarative rules (Figure~\ref{fig:example-ts-FPH}) with the inference algorithm (Figure~\ref{fig:example-FPH})
    and be surprised how close the resemblance is.

    Consider the following example where |choose| is instantiated predicatively and impredicatively:
    \begin{code}
      f = choose id  
      f :: forall alpha dot (alpha -> alpha)  -> (alpha -> alpha)                   -- predicative inst
      f :: (forall alpha dot alpha -> alpha)  -> (forall alpha dot alpha -> alpha)  -- impredicative inst
    \end{code}
    The observation underlying FPH is that impredicative instantiation may result in more than one incomparable most general System F type
    for a binding. This is undesired for reasons of modularity and predictability. FPH dictates that impredicative instantiation is forbidden
    if it has influence on the type of a binding.

    To formalize this difference, FPH introduces the concept of a box. When a bound variable is instantiated with a polymorphic type in FPH, it is enclosed
    within a box. A box expresses that the type it encloses may have been obtained through impredicative instantiation. FPH forbids the type of a
    binding to have a box in the type, thus ensuring that these possible undesired effects have no influence on the type. This absence of boxes
    can arise due to two reasons:
    \begin{itemize}
    \item The type with the box is simply not part of the type of the binding.
    \item There is an unboxing relation ($\preceq\sqsubseteq$) that allows shrinking of boxes over the monomorphic spine of a type. When we discover that
      the type in the box cannot influence the type of the result, we can remove the box.
    \item The programmer can give an explicit type signature, which does not have boxes.
    \end{itemize}
    A particular invariant maintained by FPH is that there may not be a box within a box (the ``monomorphic substitution'' operator $\coloneqq$ takes care of this).

    \begin{figure}[htp]
    Types with boxes:
    \begin{displaymath}
      \tau = \ldots \;\mid\; \tybox{\tau}
    \end{displaymath}
    Type rules:
    \begin{mathpar}
      \inferrule*[right=inst]
        { \Gamma \vdash x : \forall \overline{\alpha} . \:\tau_2 \\\\
          \tau_1 \: \mbox{unboxed iff mono} \\
        }
        { \Gamma \vdash x : [\overline{\alpha \coloneqq \tau_1}] \: \tau_2 }

      \inferrule*[right=subs]
        { \Gamma \vdash e : \tau_1 \\\\
          \tau_1 \preceq\sqsubseteq \tau_2 \\
        }
        { \Gamma \vdash e : \tau_2 }

      \inferrule*[right=app]
        { \Gamma \vdash f : \tau_f \\
          \Gamma \vdash a : \tau_a \\\\
          \removeboxes{\tau_a} \equiv \removeboxes{\tau'_a} \\
          \tau_f \equiv \tau'_a \rightarrow \tau_r \\
        }
        { \Gamma \vdash f \: a : \tau_r }

      \inferrule*[right=ann]
        { \Gamma \vdash e : \tau_1 \\\\
          \removeboxes{\tau_1} \equiv \tau_2
        }
        { \Gamma \vdash (e :: \tau_2) : \tau_2 }

      \inferrule*[right=lam.impl]
        { |mono| \:\tau_x \\\\
          x :: \tau_x, \Gamma \vdash e : \tau \\
        }
        { \Gamma \vdash \lambda x . e : \tau_x \rightarrow \tau }

      \inferrule*[right=lam.expl]
        { x :: \tau_x, \Gamma \vdash e : \tau }
        { \Gamma \vdash \lambda (x :: \tau_x) . e : \tau_x \rightarrow \tau }

      \inferrule*[right=let]
        { \Gamma \vdash e : \tau_x \\
          |noBoxes| \:\tau_x \\
          x :: \tau_x, \Gamma \vdash b : \tau \\
        }
        { \Gamma \vdash |let x = e in b| : \tau }
    \end{mathpar}
    Boxy instantiation rules:
    \begin{mathpar}
      \inferrule*[right=bi]
        {}
        { \tybox{\forall \overline{\alpha} . \tau_1}  \preceq  \tybox{[\overline{\alpha \coloneqq \tau_2}] \tau_1} }
      
      \inferrule*[right=br]
        {}
        { \tau \preceq  \tau }
    \end{mathpar}
    Protected unboxing rules:
    \begin{mathpar}
      \inferrule*[right=tbox]
        { |mono|\:\tau }
        { \tybox{\tau} \sqsubseteq \tau }

      \inferrule*[right=refl]
        {}
        { \tau \sqsubseteq \tau }

      \inferrule*[right=poly]
        {  \tau_1 \sqsubseteq \tau_2 \\\\
           |unboxed| \: \overline{\alpha} \: \tau_1 \\\\
           |unboxed| \: \overline{\alpha} \: \tau_2 \\
        }
        { \forall \overline{\alpha} . \tau_1 \sqsubseteq \forall \overline{\alpha} . \tau_2 }

      \inferrule*[right=conbox]
        { \tybox{\tau_1} \sqsubseteq \tau_3 \\
          \tybox{\tau_2} \sqsubseteq \tau_4 \\
        }
        { \tybox{\tau_1} \rightarrow \tybox{\tau_2} \sqsubseteq \tau_3 \rightarrow \tau_4 }

      \inferrule*[right=cong]
        { \tau_1 \sqsubseteq \tau_3 \\
          \tau_2 \sqsubseteq \tau_4 \\
        }
        { \tau_1 \rightarrow \tau_2 \sqsubseteq \tau_3 \rightarrow \tau_4 }
    \end{mathpar}
    \caption{The FPH type system.}
    \label{fig:example-ts-FPH}
    \end{figure}

    The type rules for FPH contain many non-deterministic aspects, especially due to the interaction between types and boxes. Both the structure of the types, and
    the demands on the boxes become only gradually available. In some cases, we may discover that the type is not allowed to have any boxes before the actual
    type becomes known. Alternatively, in case of box-stripping ($\removeboxes{\cdot}$), we may know portions of the type structure, but nothing about the boxes yet.

    \begin{figure}[htp]
    Boxy types:
    \begin{code}
      type Ty   = (Ty', Box)
      data Box  = BYes | BNo | BVar GuessVar
      data Ty'  = TArr Ty' Ty' | TGuess GuessVar | ...
    \end{code}
    Scheme declarations:
    \begin{displaymath}
    \begin{array}{lr}
    \lightbrack{\tau_1 \typeofinput_{\mbox{d}} Ty} \sqsubseteq \lightbrack{\tau_2 \typeofoutput_{\mbox{d}} Ty}   &  \lightbrack{\tau_1 \typeofinput_{\mbox{d}} Ty} \sqsubseteq' \lightbrack{\tau_2 \typeofinput_{\mbox{d}} Ty} \\
    \lightbrack{\tau_1 \typeofinput_{\mbox{d}} Ty} \preceq \lightbrack{\tau_2 \typeofoutput_{\mbox{d}} Ty}       &  \lightbrack{\tau_1 \typeofinput_{\mbox{d}} Ty} \preceq' \lightbrack{\tau_2 \typeofoutput_{\mbox{d}} Ty} \\
    \end{array}
    \end{displaymath}
    Inferencer rules:
    \begin{mathpar}
      \inferrule*[right=inst.v]
        { \Gamma \vdash_x x : \forall \overline{\alpha} . \:\tau_2 \\
          \overline{\tilde{\tau}_1} \; \mbox{fresh} \\\\
          \Defer_{b_i} \: \mbox{\begin{tabular}{l}
                                  $[\; \sembrack{v} = b,\; \Commit_{v} \: |BYes| \;]$, \\
                                  $[\; b = |BNo|,\; |mono| \: \tilde{\tau} \;]$, \\
                                  $[\; b = |BYes| \;]$ \\
                                \end{tabular}} \\
        }
        { \Gamma \vdash x : [\overline{\alpha \coloneqq \boxexpr{\tilde{\tau}_1}{b}}] \: \tau_2 }

      \inferrule*[right=subs]
        { \Gamma \vdash e : \tau_1 \\\\
          \tau_1 \preceq\sqsubseteq \tau_2 \\
        }
        { \Gamma \vdash_s e : \tau_2 }

      \inferrule*[right=gen.lazy]
        { \Gamma \vdash_s e : \tau_1 \\\\
          \ldots \\
        }
        { \Gamma \vdash_g e : \tau_2 }

      \inferrule*[right=app]
        { \Gamma \vdash_s f : \tau_f \\
          \Gamma \vdash_g a : \tau_a \\\\
          \removeboxes{\tau_a} \equiv \removeboxes{\tau'_a} \\
          \tau_f \equiv \tau'_a \rightarrow \tau_r \\
        }
        { \Gamma \vdash f \: a : \tau_r }

      \inferrule*[right=ann]
        { \Gamma \vdash_g e : \tau_1 \\\\
          \removeboxes{\tau_1} \equiv \tau_2
        }
        { \Gamma \vdash (e :: \tau_2) : \tau_2 }

      \inferrule*[right=let]
        { \Fixate_{b} \left[\; \Gamma \vdash_l e : \tau_x ,\; |noBoxes| \:\tau_x \;\right] \\\\
          x :: \tau_x, \Gamma \vdash b : \tau \\
        }
        { \Gamma \vdash |let x = e in b| : \tau }
    \end{mathpar}
    Boxy instantiation rules:
    \begin{mathpar}
      \inferrule*[right=boxy.inst]
        { b_2 \: \mbox{fresh} \\
          \Defer_{\tilde{\tau}_2} \: [\; b_1 \equiv b_2 ,\; \Force \: b_1,\: \boxexpr{\tilde{\tau}_1}{b_1} \preceq' \tau,\: \tau \equiv \boxexpr{\tilde{\tau}_2}{b_2} \;] \\
        }
        { \boxexpr{\tilde{\tau}_1}{b_1} \preceq \boxexpr{\tilde{\tau}_2}{b_2} }
      
      \inferrule*[right=bi]
        { \overline{\tau_2} \: \mbox{fresh} }
        { \tybox{\forall \overline{\alpha} . \tau_1}  \preceq'  \tybox{[\overline{\alpha \coloneqq \tau_2}] \tau_1} }
      
      \inferrule*[right=br]
        {}
        { \tilde{\tau} \preceq'  \tilde{\tau} }
    \end{mathpar}
    Protected unboxing rules:
    \begin{mathpar}
      \inferrule*
        { \tilde{\tau}_2 \: \mbox{fresh} \\
          \removeboxes{\tilde{\tau_1}} \equiv \removeboxes{\tilde{\tau_2}} \\
          \Defer_{b_2} \mbox{\begin{tabular}{l}
                                           $[\; \Let \: \sembrack{|_|} = b_2,\; b_1 \equiv b_2,\; \Force\:b_2 ,\; \boxexpr{\tilde{\tau}_1}{b_1} \sqsubseteq' \boxexpr{\tilde{\tau}_2}{b_2} \;]$, \\
                                           $[\; \Let \: \sembrack{v} = b_1,\; \Commit_{v} \: b_2,\; \boxexpr{\tilde{\tau}_1}{b_1} \sqsubseteq' \boxexpr{\tilde{\tau}_2}{b_2} \;]$, \\
                                           $[\; \boxexpr{\tilde{\tau}_1}{b_1} \sqsubseteq' \boxexpr{\tilde{\tau}_2}{b_2} \;]$ \\
                                         \end{tabular}} \\
        }
        { \boxexpr{\tilde{\tau}_1}{b_1} \sqsubseteq \boxexpr{\tilde{\tau}_2}{b_2} }

      \inferrule*[right=unbox.ty.defer]
        { \Let \: \sembrack{v} = \tilde{\tau}_1 \\\\
          \Defer_{\tilde{\tau}'_1} [ \boxexpr{\tilde{\tau}_1}{b} \sqsubseteq' \tau_2 ] \\\\
          \tilde{\tau}_1 \equiv \tilde{\tau}'_1 \\
        }
        { \boxexpr{\tilde{\tau}_1}{b} \sqsubseteq' \tau_2 }

      \inferrule*[right=tbox]
        { |mono|\:\tilde{\tau} }
        { \tybox{\tilde{\tau}} \sqsubseteq' \tilde{\tau} }

      \inferrule*[right=refl]
        {}
        { \boxexpr{\forall \overline{\alpha} . \tau}{b} \sqsubseteq' \boxexpr{\forall \overline{\alpha} . \tau}{b} }

      \inferrule*[right=poly]
        {  \tau_1 \not\equiv \tau_2 \\
           \tau_1 \sqsubseteq \tau'_2 \\
           \tau'_2 \equiv \tau_2 \\\\
           |unboxed| \: \overline{\alpha} \: \tau_1 \\
           |unboxed| \: \overline{\alpha} \: \tau_2 \\
        }
        { \forall \overline{\alpha} . \tau_1 \sqsubseteq' \forall \overline{\alpha} . \tau_2 }

      \inferrule*[right=conbox]
        { \tybox{\tilde{\tau}_1} \sqsubseteq \tau_3 \\
          \tau_3 \equiv \tau'_3 \\\\
          \tybox{\tilde{\tau}_2} \sqsubseteq \tau_4 \\
          \tau_4 \equiv \tau'_4 \\
        }
        { \tybox{\tilde{\tau}_1} \rightarrow \tybox{\tilde{\tau}_2} \sqsubseteq' \tau'_3 \rightarrow \tau'_4 }

      \inferrule*[right=cong]
        { \tilde{\tau}_1 \sqsubseteq \tau_3 \\
          \tau_3 \equiv \tau'_3 \\\\
          \tilde{\tau}_2 \sqsubseteq \tau_4 \\
          \tau_4 \equiv \tau'_4 \\
        }
        { \tilde{\tau}_1 \rightarrow \tilde{\tau}_2 \sqsubseteq' \tau'_3 \rightarrow \tau'_4 }
    \end{mathpar}
    Semantics:
    \begin{code}
    instance Unifyable Box where ...
    instance Deferrable Box where ... mkFixedValue = const BNo
    \end{code}
    \caption{Inferencer for FPH.}
    \label{fig:example-FPH}
    \end{figure}

    \paragraph{Idea.}
    We choose here to be able to guess the type independent of the boxes.
    Each alternative of a type gets a box annotation. Types $\tau$ are of the form $\boxexpr{\tilde{\tau}}{b}$,
    where $\tilde{\tau}$ is a regular type with types $\tau$ as components, and $b$ a box annotation. A box annotation is
    either concrete (|BYes| or |BNo|), or a guess. Types in the environment have box-annotations |BNo|, and box-annotations
    |BYes| are introduced by instantiation. The unboxing rules then relate boxes to types, and eliminate boxes as soon as
    more type information becomes available. This unboxing (see subscript $s$ and $\RefTirName{Subs}$) is done for each
    sub-expression. For an application, only the instantiation of the function type can cause boxes to appear in the result
    type. Possible boxes in the type of the argument are stripped away (these would otherwise cause boxes inside boxes).
    Also, annotations are considered safe and causes boxes to disappear. Finally, at a let-binding, we first generalize
    and fixate the guesses in the types, and only then fixate the boxes. This ensures that when fixating the boxes, we
    know that choices of the boxes do not influence the types in the local scope anymore.

    Note the following notation. A $\sembrack{v}$ represents a guess containing variable $v$ (produced or obtained by means of |mkDeferValue| or |matchDeferValue| respectively).
    A type $\tilde{\tau}$ (|Ty'|) at a place where a $\tau$ (|Ty|) is expected represents a pair of $\tilde{\tau}$ with a box annotation of |BNo|. A type
    $\tybox{\tilde{\tau}}$ represents a pair of $\tilde{\tau}$ with a box annotation of |BYes|.

    The boxy-instantiation rules allows instantiation inside an outermost box. The application of these rules is
    controlled by $\RefTirName{Boxy.Inst}$, as follows. Decisions are delayed until more is known about the
    result type. Then we force the decisions to have been made about a potential box surrounding it. We then know which
    one the two actual instantiation rules is applicable. Note that we are not afraid of instantiation: the
    $\RefTirName{Gen.Lazy}$ generalizes again if needed.
    
    For protected unboxing, rule $\RefTirName{unbox}$ controls how the rules are applied. It ensures that the input and output type
    are matched together, disregarding boxes such that this information flow is independent of the information flow about boxes. Then,
    applying the actual rules is deferred until the two box-annotations have been resolved. In case $b_2$ is still unknown, we default
    it. If $b_2$ is known, but $b_1$ is not, we apparently have freedom in the choice and choose $b_2$ for $b_1$.
    Finally, if we are in the situation that we know the annotations but not the type, we delay resolving the unboxing until we know the
    type by means of $\RefTirName{unbox.ty.defer}$.

    \begin{figure}[htp]
    Box operations:
    \begin{code}
    noBoxes :: Ty -> I ()
    noBoxes (t,b) = do  unif b BNo
                        traverse noBoxes t
    \end{code}
    \begin{code}
    unboxed :: GuessVar -> Ty -> I ()
    unboxed a (t,b)  | a `elem` ftv t  = do  unif b BNo
                                             traverse (unboxed a)
                     | otherwise       = return ()
    \end{code}
    \begin{code}
    striproot = strip tau
    strip :: Ty -> Ty
    strip (t,_) = do  t' <- dtraverse strip t; return (t', BNo)
    \end{code}
    \begin{code}
    dtraverse f = dwrap (traverse f)
    traverse f (TArr t1 t2)  = do  t3 <- f t1; t4 <- f t2
                                   return (TArr t3 t4)
    traverse f (TAll vs t1)  = do  t2 <- f t1; return (TAll vs t2)
    traverse f t             = return t
    \end{code}
    \begin{code}
    mono t = unif t (dcheck t)
    dcheck = dwrap check
    check (TArr t1 t2)  = check' t1 >> check' t2
    check (TAll _ _)    = fail "not a mono type"
    check _             = return ()
    check' (t,_)        = dcheck t
    \end{code}
    \caption{FPH monadic Haskell premises}
    \label{fig:example-premisses}
    \end{figure}

    The monadic Haskell expressions used in the premises of the inferencer rules are given in Figure~\ref{fig:example-premisses}. |noBoxes| forces
    the absence of boxes everywhere in the type, and |unBoxed| only on the spine to each occurrence of type variable |a|. The most involving, however,
    is box-stripping. It produces a type with all boxes removed, without affecting the original box annotations. The difficulty is that the type may
    not be fully known yet. Fortunately, we can use |defer|, |equal| and |commit| in monadic expressions too. In fact, we can write higher-order functions
    to factor out some patterns. For example, |dwrap| (Figure~\ref{fig:example-abstraction}) factors out all the non-determinism of a recursive function
    where the input is equal to the output modulo some guesses.

    \begin{figure}[htp]
    \begin{code}
      dwrap :: forall alpha dot Deferrable alpha => (alpha -> I alpha) -> alpha -> I alpha
      dwrap f t = do  (tOut,()) <- defer (\tIn _ ->
                        do  unifOne t tIn
                            t' <- f t
                            unif tOut t'
                      return tOut
    \end{code}
%%    refine :: forall alpha dot Unifyable alpha => ( alpha ->
%%                  ( forall beta dot Deferrable beta => ((alpha -> I alpha) -> beta -> I beta) -> beta -> I beta )
%%                  -> I alpha ) -> alpha -> I alpha
%%      refine f = f'
%%        where f' t =  f t (\g s ->
%%                        do  (sOut,()) <-  defer (\sIn _ ->
%%                                            do  commit' s sIn
%%                                                sOut' <- g f' s
%%                                                equal sOut sOut' )
%%                            return sOut
%%              commit' sembv t  = commit v t
%%              commit' t1 t2    = unify (\_ _ -> return ()) t1 t2
    \caption{Example of evaluation control abstraction.}
    \label{fig:example-abstraction}
    \end{figure}

    \paragraph{Other type systems.}
    The syntax of the $\Defer$-statement is actually a bit more general than we presented in these examples. In the examples,
    the deferred statements did not have outputs, only inputs. We allow the deferred statements to have outputs. For example,
    another type system for first class polymorphism, HML~\cite{DBLP:conf/popl/Leijen09}, requires deferred statements that
    produce a prefix |Q| (denoted with $\Defer_{\tau}^{Q} [ \ldots ]$).
    
    Although our examples were about inferencers for type systems dealing with polymorphism, we stress that these were chosen
    in order to pave the way to a complex example, and that we are not limited to such type systems.


\section{Related Work}
\label{sect:related-work}

    We present an extension of our previous work on the Ruler system~\cite{DBLP:conf/flops/DijkstraS06}.
    In this system, type rules are required to be written as deterministic functions, and both a type-setted \LaTeX~document
    and an efficient Attribute-Grammar based inferencer are derived from them. One of the goals of this
    system is to close the gap between formal description and implementation. However, non-deterministic aspects
    cannot be directly described in this system, and are omitted (to be solved externally). Thus, this leaves the gap
    wide open, and we close it with this work.

    \paragraph{Functional Logic Programming.}
    The essential problem with non-deterministic aspects is that the function to resolve it needs to make decisions,
    but is unable to do so based on what is known about the inputs at that point. Therefore, the idea is to delay
    the execution until we know more about the output, and let expected output play a role in the decision process.
    Therefore, at specific places, we turn functions into relations, which has a close resemblance to
    Functional Logic Programming~\cite{DBLP:journals/jlp/Hanus94}.

    With FLP, non-deterministic functions can be written as normal functions. The possible
    alternatives that these functions can take depends not only on the inputs, but also on the scrutinizing of the
    result. With evaluation strategies such as narrowing~\cite{DBLP:conf/alp/Antoy97}, the search space is explored
    in a demand-driven way. Knowledge of context is pushed inwards, reducing possible alternatives, and causing
    evaluation to occur that refines the context even more.

    With the $\Defer$ and $\Commit$, we offer a poor man's mechanism to FLP. The delaying of choice and the
    scrutinizing of the choice is explicit. A $\Commit$ is required to reduce the choice to at most one
    possibility. Yet, we have good reasons not to support the full generality of FLP. We want to integrate the
    inferencers specified in our language into mainstream compilers. Our approach makes only little demands on
    infrastructure. If it can cope with Algorithm W~\cite{DBLP:journals/jcss/Milner78}, then the proposed mechanisms
    of this paper fit. Furthermore, we want to be able to use constraint solvers in some foreign language, or arbitrary
    Haskell libraries in our inference rules. This gives rise to problems with narrowing.

    A difference with respect to FLP is our fixation and inspection of guesses. Consider an expression like |const x y|.
    For such an expression, the type of |y| is irrelevant and will not be scrutinized. However, we have several reasons to
    do so. We produce derivations, so we need a derivation of |y|. Decisions about the inference of |y| need to be made, even
    when the context does not make strong demands about which one. To make such a decision, we need to inspect which values
    are still guesses. As a consequence, more type information may be discovered, or even type errors that would otherwise go
    unnoticed, or which only arise much later (say, when generating code). Also, to deal with
    rules such as generalization properly, we need to know the difference between an unresolved guess and some fixed but
    unconstrained information. Furthermore, unresolved guesses retain memory which causes severe memory problems when growing
    unchecked in mainstream compilers, and when integrating with foreign code, we need invariants about which parts of values
    are resolved. Finally, examples such as in Figure~\ref{fig:example-queing} really require the guessing to be explicit and
    first class.

    \paragraph{Logic Programming}
    In a similar way as with FLP, our approach has strong ties with Logic Programming. One particular difference is that we
    disallow backtracking and of all possible rules demands that only one rule can succeed. Again, the reasons are related to
    efficiency and integration. However, there is an even more important reason: to be able to produce sensible type errors,
    and to prevent infinite searches in the presence of type errors.

    \paragraph{Inferencer Frameworks.}
    There are inferencer frameworks such as |HM(X)|~\cite{DBLP:journals/jfp/SulzmannS08}, which is based on a fixed set of type
    rules parametrized over some relation |X|, for which an inference algorithm needs to be given to obtain an inferencer for
    the full language. Such a framework is in fact orthogonal to Ruler, and Ruler can be used to construct the algorithm for
    |X|. In fact, the precise relation of Ruler to other constraint-based inference frameworks is that a Ruler specification
    can be seen as both a specification of constraints, annotated with the algorithm to solve them.

    \paragraph{Type rule tools.}
    OTT~\cite{DBLP:conf/icfp/SewellNOPRSS07} produces code for proof assistants. SASyLF~\cite{1411266} is such a proof assistant
    tailored to proving properties about type systems, as is Twelf~\cite{DBLP:journals/jfp/HarperL07}.
    
    Tinkertype~\cite{DBLP:journals/jfp/LevinP03} is a system that can also produce inferencers from type rules. However, the
    inference algorithms are not derived from the type rules. Instead, it depends on a repository with code for each relation
    to compose the inferencer.


\section{Type Inferencer Syntax}
\label{sect:syntax}

  \subsection{Core Syntax}

    The syntax of the type inferencer language (named \RulerCore) is given in
    Figure~\ref{fig:ruler-core-syntax}. A type inferencer is a triple $(\Schemes,\Rules,\Haskell)$
    of schemes $\Schemes$, rules $\Rules$, and some Haskell support code in the form of
    data-type declarations, some instances for them, and utility functions. A scheme $\Scheme$
    represents a function named $\SchemeName$ with inputs declared by environment $\Env_{\In}$, and
    outputs by environment $\Env_{\Out}$. A scheme can be parametrized over some types $\alpha$,
    which provides for a limited form of polymorphism for the inferencer rules.
    The inferencer rules in $\Rules$ with scheme name $\SchemeName$
    define the function $\SchemeName$. Each rule $\Rule$ consists of a (possibly empty) sequence of
    premises ($\Statement$), and a conclusion ($\Statement_{\SchemeName}$).

    It is important to realize that we are not defining type rules here. Schemes are not
    arbitrary relations, but are functions. The premises are statements, not predicates. Also,
    the order of the premises matter. Values for all inputs need to be available before
    a scheme can be instantiated. The rules and statements have a certain operational behavior.
    A rule evaluates successfully if and only evaluation of all its premises succeeds, and for
    each statement we give a brief description below. We make this more precise later.

    The conclusion $\Rule_{\SchemeName}$ of an inferencer rule defines to what names the actual
    parameters of the scheme $\SchemeName$ are bound in the context of the rule, and which
    local results are the outputs of the scheme. Expressed with bindings $\Bindings_{\In}$ and
    $\Bindings_{\Out}$ respectively, where bindings are a mapping from formal name to actual
    name.

    \begin{figure}[htp]
    \begin{displaymath} 
      \begin{array}[t]{lrlr}
        \Schemes                   &    =    &   \Scheme_1, \ldots, \Scheme_k                                                 & (\mbox{schemes})\\
        \Scheme                    &    =    &   \forall \overline{\alpha} .\;\; \Env_{\In} \vdash_{\SchemeName} \Env_{\Out}  & (\mbox{scheme})\\
        \Rules                     &    =    &   \Rule_{\SchemeName_1}, \ldots, \Rule_{\SchemeName_k}                         & (\mbox{rules})\\
        \Rule_\SchemeName          &    =    &   \Statements \; ; \; \Statement_\SchemeName                                   & (\mbox{rule})\\
        \Statements                &    =    &   \Statement_1, \ldots, \Statement_k                                           & (\mbox{statements})\\
        \Statement                 &    =    &   \Statement_{\SchemeName}                                                     & (\mbox{instantiate})\\
                                   &    \mid &    n_1 \equiv n_2                                                              & (\mbox{unification})\\
                                   &    \mid &   \Execution \; e :: \Idents{n} \rightarrow \Idents{m}                         & (\mbox{execution})\\
                                   &    \mid &   \Fixpoint_{\Bindings}^{\Idents{n}} \; \Statement_{\SchemeName}               & (\mbox{fixpoint})\\
                                   &    \mid &   \Defer_n^{\Idents{m}} \; \Statements_1, \ldots, \Statements_k                & (\mbox{defer})\\
                                   &    \mid &   \Commit_{v_{\Type}} \; n                                                     & (\mbox{commit})\\
                                   &    \mid &   \Fixate_{\Type} \: \Statements                                               & (\mbox{fixate})\\
        \Statement_\SchemeName     &    =    &   \Bindings_{\In} \vdash_\SchemeName \Bindings_{\Out}                          & (\mbox{scheme instance})\\
        \Env                       &    =    &   n_1 ::_{\Prop} \Type_1, \ldots, n_k ::_{\Prop} \Type_k                       & (\mbox{environment})\\
        \Bindings                  &    =    &   n_1 \mapsto m_1, \ldots, n_k \mapsto m_k                                     & (\mbox{bindings})\\
%        \Props                     &    =    &   \Prop_1, \ldots, \Prop_k                                                    & (\mbox{properties})\\
        \Prop                      &    =    &   \mbox{d}                                                                     & (\mbox{deferrable})\\
                                   &    \mid &   \mbox{u}                                                                     & (\mbox{unifyable})\\
                                   &    \mid &   \emptyset                                                                    & (\mbox{none})\\
      \end{array}
    \end{displaymath}
    With scheme names $\SchemeName$, identifiers $n$, $m$, and $v$, collection of identifiers $\Idents{n}$ and $\Idents{m}$, Haskell types $\tau$,
    and expressions e.
    \caption{Syntax of \RulerCore.}
    \label{fig:ruler-core-syntax}
    \end{figure}

    Statements for premises come in different forms. There are a couple of statements that allow algorithms
    to be described:
    \begin{itemize}
    \item Evaluation of statement $\Statement_{\SchemeName}$
      instantiates a scheme, which means applying the scheme function to the inputs, and obtaining the
      outputs if this application is successful. For the input values, the values bound to the actual names
      are taken for the formal names (specified by bindings $\Bindings_{\In}$). Similarly, the values bound
      to the formal name of the scheme are made available under the actual name (specified by bindings
      $\Bindings_{\Out}$).
    \item The unification statement attempts to unify the values bound to the names $n_1$ and $n_2$.
      A unification algorithm based on structural equality needs to be available for the values to which
      these two types are bound. Successful unification results in a substitution that makes the two values
      equal. This substitution is implicit and can be assumed to be applied everywhere.
    \item The execution statement allows monadic Haskell code to be executed. This code is a function taking
      the values bound to names $\Idents{n}$ as parameter and returns a monadic value containing values for outputs
      $m^*$. We consider this expression language in more detail later. The purpose of these execution statements
      is to perform the actual computations needed to produce the values for the inputs of a scheme instantiation,
      and to inspect the outputs of it by means of pattern matching.
    \item The fixpoint statement repeatedly instantiates $\SchemeName$, as long as the values bound to
      identifiers $\Idents{n}$ change. The bindings $\Bindings$ specify how the outputs are mapped back to
      the inputs after each iteration.
    \end{itemize}
    The statements that allow us to algorithmically deal with non-deterministic aspects:
    \begin{itemize}
    \item The defer statement represents one of the sequence of statements $\Statements_i$, except that evaluation of it
      takes place at a later time. In the meantime, a guess (encoded as a fresh variable) for the output $n$ is
      produced, and bottom-values for outputs $\Idents{n}$. For each guessable data type, we require that we can encode a
      variable as a value of this data type (denoted as $\sembrack{v}$). These guesses can be passed around as normal values.
    \item A commit statement refines a guess bound to $v$ with the value bound to identifier $n$, and runs
      deferred statements, which may lead to other refinements of guesses.
    \item The fixate statement executes statements $\Statements$. All guesses of type $\Type$ that were not committed during
      this execution are resolved by executing the deferred statements. Any remaining guesses are marked as fixed. These
      now represent opaque values that cannot be refined anymore.
    \end{itemize}
    
    Expressions in an $\Execution$-statement are Haskell functions in monad |I| that get the inputs passed as arguments and are
    obliged to return a product with the results. Hence the type of an expression |e|:
    \begin{displaymath}
      e :: \tau_{n_1} \rightarrow \ldots \tau_{n_k} \rightarrow I (\tau_{m_1}, \ldots, \tau_{m_l})
    \end{displaymath}
    
    The |I| monad contains a hidden state, and support failure. In particular, the following operations are available:
    \begin{code}
    commit   :: Deferrable alpha => alpha -> alpha -> I ()
    defer    :: (Deferrable alpha, Prod beta) => (alpha -> I beta) -> I (alpha, beta)
    unif     :: Unifyable alpha => alpha -> alpha -> I ()
    unifOne  :: Unifyable alpha => alpha -> alpha -> I ()
    update   :: Container alpha => alpha -> I alpha
    fail     :: String -> I ()
    \end{code}
    We create a deferrable computation with |Defer|. It takes a monadic function that is only executed when a commit
    is performed on alpha. This monadic function produces the values for the product |beta|. Until this actually
    happened, the contents of the product may not be touched. The |unif| operation enforces structural equality
    between values |alpha|. In case of |unifOne|, only structural equality on the heads of the values. Finally,
    |update| brings all guesses in |alpha| up to date, and |fail| causes the inference to fail with a type error.

%%    \begin{figure}[htp]
%%    \begin{displaymath} 
%%      \begin{array}[t]{lrlr}
%%        e  &    =    &   \lambda n \; . \; e                                   &  (\mbox{lambda}) \\
%%           &    \mid &   |do| \; m_1; \ldots; m_k; r                           &  (\mbox{monadic statements}) \\
%%        m  &    =    &   m_{\Haskell}[m]                                       &  (\mbox{monadic Haskell}) \\
%%           &    \mid &   \Unify \; e_{\Haskell,1} \;\; e_{\Haskell,2}          &  (\mbox{unify}) \\
%%           &    \mid &   (n, \overline{m}) \leftarrow \Defer \; e              &  (\mbox{defer}) \\
%%           &    \mid &   \Commit \; v \;\; e_{\Haskell}                        &  (\mbox{commit}) \\
%%           &    \mid &   n \leftarrow \Update \; e_{\Haskell}                  &  (\mbox{app subst}) \\
%%           &    \mid &   \Fail                                                 &  (\mbox{fail}) \\
%%           &    \mid &   \Abort \; e_{\Haskell}                                &  (\mbox{error}) \\
%%        r  &    =    &   \Return \; (e_{\Haskell,1}, \ldots, e_{\Haskell,k})   &  (\mbox{monadic return})
%%      \end{array}
%%    \end{displaymath}
%%    With monadic Haskell expressions $m_{\Haskell}$, and plain Haskell expressions $e_{\Haskell}$.
%%    \caption{Syntax of expressions.}
%%    \label{fig:monadic-syntax}
%%    \end{figure}
%%
%%    Expressions in the statements are a form of monadic Haskell functions. Figure~\ref{fig:monadic-syntax}
%%    lists the syntax of these expressions. The structure of these expressions is a sequence of lambdas
%%    corresponding to the inputs of the expression, and then a monadic function returning a $n$-tuple of which
%%    the components correspond to the outputs of the expression. A monadic function $m$, may be an
%%    arbitrary Haskell expression $m_{\Haskell}$ (of which monadic sub-expressions are allowed to be
%%    expressions $m$ again), or a number of special monadic operations:
%%    \begin{itemize}
%%    \item Unify performs unification on the result of the two expressions received as parameter.
%%    \item Commit specializes the guess indicated by v the the result of expression.
%%    \item Update binds a value to $n$, which is the result of the expression with all guesses updated with
%%      the most recent information available about them. After a Unify or Commit, some more information
%%      about guesses may have been obtained.
%%    \item Fail causes the corresponding execution-statement to fail. Consequently, the rule executing this
%%      statement fails too. If there is no other rule to succeed, the failure becomes a type error.
%%      Alternatively, Abort causes the type inference to abort with a type error of which the message is
%%      the result of $e_{Haskell}$. The inferencer rules must be syntax directed, which means that based on
%%      the syntax of the input to a scheme, there is exactly one rule that does not fail its pattern matches.
%%    \end{itemize}
%%    Identifiers of inferencer rules cannot be directly addressed by the Haskell expressions. Those declared
%%    as inputs can be accessed through the Haskell identifier of the corresponding lambda.
%%  
%%    All expressions are evaluated lazily, but all inputs and outputs to expressions, as well as arguments
%%    to Unify, Commit, and Update, are fully evaluated to normal form.

  \subsection{Syntactic Sugar}
  
    The previous section gave the core syntax for the type inferencer. For practical and didactic purposes,
    the examples in the previous section where given in a somewhat more convenient syntax that can be
    translated to the core syntax.
    
    First of all, we assume a series of notational conventions involving sequences (lists), environments (maps), or sets:
    \begin{itemize}
    \item A sequence of, for example, statements is denoted by $\Statements = \overline{\Statement_i} = \Statement_1 \ldots \Statement_k$,
      where $i$ ($1 \leq i \leq k$) is some index, and $k$ is left implicit as it is clear from the context, i.e. when
      $i$ ranges also over some list.
    \item A list of identifiers is denoted by $\Idents{n} = \overline{n_i} = n_1, \ldots, n_k$.
    \item The empty map, empty list, or empty set is written as $\emptyset$.
    \end{itemize}
    
    \begin{figure}[htp]
    \begin{displaymath} 
      \begin{array}[t]{lrlr}
        \Schemes                   &    =    &   \Scheme_1, \ldots, \Scheme_k                                          & (\mbox{schemes})\\
        \Scheme                    &    =    &   \forall \overline{\alpha} .\;\;  \SchemeName : d_1, \ldots, d_k       & (\mbox{scheme signature})\\
        d                          &    =    &   \mathbf{kw}                                                           & (\mbox{keyword decl})\\
                                   &    \mid &   n \typeofinput_{\Prop} \tau                                           & (\mbox{input decl})\\
                                   &    \mid &   n \typeofoutput_{\Prop} \tau                                          & (\mbox{output decl})\\
        \Rules                     &    =    &   \Rule_{\SchemeName_1}, \ldots, \Rule_{\SchemeName_k}                  & (\mbox{rules})\\
        \Rule_\SchemeName          &    =    &   \inferrule{\Statement_1 \ldots \Statement_k}{\Statement_\SchemeName}  & (\mbox{rule})\\
        \Statement                 &    =    &   \Statement_{\SchemeName}                                              & (\mbox{instantiate})\\
                                   &    \mid &    m_{\Haskell,1} \equiv m_{\Haskell,2}                                 & (\mbox{unification})\\
                                   &    \mid &   \Let \; p_{\Haskell} = e_{\Haskell}                                   & (\mbox{pure})\\
                                   &    \mid &   p_{\Haskell} \leftarrow m_{\Haskell}                                  & (\mbox{monadic bind})\\
                                   &    \mid &   m_{\Haskell}                                                          & (\mbox{monadic exec})\\
                                   &    \mid &   \Fixpoint_{\Bindings}^{\Idents{n}} \; \Statement_{\SchemeName}        & (\mbox{fixpoint})\\
                                   &    \mid &   \Defer_n^{\Idents{m}} \; \Statements_1, \ldots, \Statements_k         & (\mbox{defer})\\
%%                                   &    \mid &   \Onebyone_{n_1,n_2}^{\Idents{m}} \;\Statement_{\SchemeName}           & (\mbox{one-by-one})\\
                                   &    \mid &   \Commit_{v_{\Type}} \; e_{\Haskell}                                   & (\mbox{commit})\\
                                   &    \mid &   \Fixate_{\Type} \: \Statements                                        & (\mbox{fixate})\\
                                   &    \mid &   \Force \: m_{\Haskell}                                                & (\mbox{force})\\
        \Statement_\SchemeName     &    =    &   \SchemeName : i_1, \ldots, i_k                                        & (\mbox{scheme instance})\\
        i                          &    =    &   \mathbf{kw}                                                           & (\mbox{keyword})\\
                                   &    \mid &    p_{\Haskell}                                                         & (\mbox{value deconstruction})\\
                                   &    \mid &    e_{\Haskell}                                                         & (\mbox{value construction}) \\
      \end{array}
    \end{displaymath}
    With keywords $\mathbf{kw}$, Haskell patterns $p_{\Haskell}$, Haskell expressions $e_{\Haskell}$, and monadic Haskell expressions $m_{\Haskell}$.
    \caption{Syntax of \RulerBase.}
    \label{fig:ruler-base-syntax}
    \end{figure}
    
    The syntactic sugar is given in Figure~\ref{fig:ruler-base-syntax}. A scheme declaration $\Scheme$ for a
    scheme named $\SchemeName$, now consists of a sequence of either an input or output declaration, or a keyword.
    For example, the scheme declaration for a scheme $\mathsf{tp\_expr}$:
    \begin{displaymath}
      \mathsf{tp\_expr: } \; (\Gamma \typeofinput \mathsf{Env}) \vdash (e \typeofinput \mathsf{Expr}) : (\tau \typeofoutput_{\mbox{d}} \mathsf{Type})
    \end{displaymath}
    defines two inputs $\Gamma$ and $e$ with types $\mathsf{Env}$ and $\mathsf{Expr}$ respectively, and an
    output named $\tau$ with type $\mathsf{Type}$ (and the deferrable-property). To instantiate this scheme, the statement has the form:
    $\mathsf{tp\_expr: } \; \ldots \vdash \ldots : \ldots$, where at the places of the dots there is a
    Haskell pattern for an output, and a pure Haskell expression for an input (the reverse for the conclusion
    statement). The identifiers of a pattern can be referenced by expressions of subsequent statements of a rule. In the examples of
    Section~\ref{sect:examples}, we left out the scheme names, because it is clear from the context.

    The essential differences between Figure~\ref{fig:ruler-core-syntax} and Figure~\ref{fig:ruler-base-syntax} are:
    \begin{itemize}
    \item The syntactic sugar allows for Haskell expressions and patterns at places where originally only identifiers were
      expected. This syntactic sugar is translated to execution-statements with the appropriate inputs and outputs.
      Pattern match failures are translated to fail-expressions. We will also assume that pure Haskell expressions are automatically lifted into
      a monadic expression when needed. Also, an identifier occurring at multiple input-locations is replaced with unique identifiers with the
      necessary |equiv|-statements added to the front of the statement sequence.
    \item No special rules about the structure of monadic functions. These are normal Haskell monadic expressions in some
      monad $I$, and the commit, unify, and update-operations are functions that act in this monad.
    \item For the core language, only one deferred statement-sequence is allowed to succeed when triggered to evaluate. Here, we
          assume that more than one is allowed to succeed, but the first one from the left is taken.
    \item $\Force$ is syntactic sugar for executing f $m_{\Haskell}$. If the result is a guess, then a commit is done with a fresh
      value as parameter. If the result is still this fresh value, a commit is done with a fixed value. Otherwise, the result is ignored.
%%    \item A statement of the form $\Onebyone_{n_1,n_2}^{\Idents{m}} \; \Statement_{\SchemeName}$ is translated to:
%%          \begin{displaymath}
%%          \Defer_{n_2}^{\Idents{m}} \!\left.  \begin{array}{l} \{ |do checkConcrete|\;n_2,\; \Statement_{\SchemeName} \}\\
%%                                                               \{ \Let\; \sembrack{v} = n_2,\; |do checkConcrete| \;n_1,\; \Statement_{\SchemeName} \} \\
%%                                                               \{ \Let\; (\sembrack{v}, \sembrack{v_2}) = (n_1, n_2),\; w\:\mbox{fresh},\; \Commit_{v_2}\;w,\; \Statement_{\SchemeName} \} \\
%%                                              \end{array} \right.
%%          \end{displaymath}
%%          A one-by-one statement defers execution of $\Statement_{\SchemeName}$ until more is known about $n_2$. Then there are
%%          three situations to consider. If parameter $n_2$ is not a guess, then some concrete information is found about $n_2$
%%          and we proceed iwth the execution of $\Statement_{\SchemeName}$. Otherwise, we have not found out anything, and are
%%          defaulting. In that case, either $n_1$ has a concrete value and we can continue, otherwise we force the defaulting of
%%          $n_1$ first.
%%          The idea behind the one-by-one statement is that each recursive step in $\Statement_{\SchemeName}$ should go through
%%          this one-by-one statement again, such that a derivation is constructed one level at a time (on an on-demand basis).
%%          (note to self: wouldnt that be something to be done by fixate automatically...)
    \end{itemize}
    Identifiers occurring in expressions are brought up-to-date with respect to guesses just before evaluating the expressions.

    \begin{figure}[htp]
    \begin{code}
      pattern Map String Ty where
        x :: tau, Gamma     input   insert x tau Gamma

      pattern Expr where
        x                   output  EVar x
        f a                 output  EApp f a
        \x dot e            output  ELam x e
      
      pattern Ty where
        [alpha := ty1] ty2  output  singleSubst alpha ty1 |=> ty
      
      pattern Ty where
        boxexpr t b         input   (t, b)
        tybox t             input   (t, BYes)
        t                   input   (t, BNo)
    \end{code}
    \caption{Examples of pattern declarations.}
    \label{fig:ruler-pattern-example}
    \end{figure}

    Finally, we assume a number of pattern declarations, of which an example is given in Figure~\ref{fig:ruler-pattern-example}. These
    define special syntax for Haskell expressions (indicated with |input|) and patterns (indicated with |output|) for certain
    specific types, and the translation to Haskell. To disambiguate, the types of identifiers play a role. The pattern |x| can
    stand for the identifier x (say, if the type is |String|), or for the expression |EVar x| if type is |Expr|.

\section{Operational Semantics}
\label{sect:operational-semantics}

%% (add something about having an executable Haskell version of the rules introduced in this section. Add something about the
%% declarative nature of these rules. Add something about the possibility to write these rules in the proposed language itself,
%% except that this would be more confusing to the reader).

In this section we give a big-step operational semantics of the inferencer language introduced in Section~\ref{sect:syntax}.
We first discuss some notation, then explain the evaluation rules. Evaluation of the inferencer rules involves data manipulation.
Some demands are made about the data in question. In particular, we require structural equality to be defined for data types.
We finish this section with a discussion of these demands.

  \subsection{Notation}

    For the operational semantics, heaps $\Heap$, substitutions $\Substitution$ and derivations $\Deriv$ are used for
    bookkeeping. Their syntax is given in Figure~\ref{fig:subst-syntax}. Heaps are a mapping of locations (in our case, plain identifiers) to Haskell values.
    Substitutions keep track of information about guesses (identified by a variable $v$). Either a guess is resolved and represents
    some concrete value $w$ of type $\tau$, or will be resolved through a commit on another variable and is for the moment mapped
    to $\bot$, or represents a closure of the deferred statements. In the latter case, we store in a heap $\Heap$ entries for each
    identifier referenced by the deferred statements, store a scope identifier $\Scope$ representing the deepest scope in which the
    deferred statement is introduced (encoded as a number equal to the nesting-depth), and a rule identifier $r$. Each defer-statement
    introduces a unique rule identifier, which is
    a placeholder for a derivation. Derivations represents a partial derivation in an abstract way. The conclusions of each rule make
    up the nodes of the derivation-tree (with the values of their instantiation in heap $\Heap$). Statements that cannot be represented
    by this are represented with an opaque-leaf $\diamond$. These derivations may be partial and refer to sub-derivations named
    $\RuleIdent$ with $\derivref{\RuleIdent}$.

    \begin{figure}[htp]
    \begin{displaymath} 
      \begin{array}[t]{lrlr}
      \Heap           &    =    &   n_1 \coloneqq w_{1,\Type_1}, \ldots, n_k \coloneqq w_{k,\Type_k}                    & (\mbox{heap})\\
%%      h               &    =    &   w_{\Type}                                                                           & (\mbox{concrete value})\\
%%                      &    \mid &   v_{\mathsf{deferred}}                                                               & (\mbox{deferred value})\\
%%                      &    \mid &   v_{\mathsf{fixed}}                                                                  & (\mbox{fixed value})\\
      \Substitution   &    =    &   v_1 \mapsto q_1, \ldots, v_k \mapsto q_k                                            & (\mbox{substitution})\\
      q               &    =    &   w_{\Type}                                                                           & (\mbox{subst value})\\
                      &    \mid &   \bot                                                                                & (\mbox{subst bot})\\
                      &    \mid &   \Deferred_{\Heap}^{\Scope,\RuleIdent} \; \Statements_1, \ldots, \Statements_k       & (\mbox{deferred})\\
      \Deriv          &    =    &   \inferrule{\Deriv}{\Heap \vdash c_s}                                                & (\mbox{node})\\
                      &    \mid &   \derivref{\RuleIdent}                                                               & (\mbox{reference})\\
                      &    \mid &   \diamond                                                                            & (\mbox{opaque})\\
                      &    \mid &   \Deriv_1 \; \Deriv_2                                                                & (\mbox{and-cons})\\
      \DerivsEnv      &    =    &   \RuleIdent_1 \mapsto \Deriv_1, \ldots, \RuleIdent_k \mapsto \Deriv_k                & (\mbox{named derivations})\\
      \end{array}
    \end{displaymath}
    With Haskell value $w$, rule identifier $\RuleIdent$, and scope identifier $\Scope$.
    \caption{Syntax of heaps, substitutions, and derivations.}
    \label{fig:subst-syntax}
    \end{figure}
    
    Substitutions satisfy the usual substitution properties. Juxtaposition of substitutions $\Substitution_1 \Substitution_2$ represents the
    left-biased union of the two substitutions, with $\Substitution_1$ applied to all entries $q$ of $\Substitution_2$. Applying a
    substitution $\Substitution_1$ to a value $w$, denoted with $\Substitution w$, replaces each guess
    $\sembrack{v}$ with either $w$ if $v \mapsto w_{\Type} \in \Substitution$ or itself otherwise. Application to a $\bot$-entry is the
    identity, and to a $\Deferred$-entry means applying it to the heap. Substitution application is lifted to environments, heaps, and
    derivations as well.
    
    We also use some notation concerning heaps and bindings. The lookup of a value for an identifier is written as $\Heap(n)$. With
    bindings we can take and rename entries in a heap: $\Heap(\Bindings)$ is a heap which for each binding $n \mapsto n'$ has the value
    $w_{\Type}$ for $n$ taken from $\Heap$ as $n'$, i.e. $\Heap(\Bindings)(n) = \Heap(n')$. We also use the reverse:
    $\Bindings(\Heap)(n') = \Heap(n)$. Juxtaposition of heaps stands for the left-biased union of the two.

  \subsection{Evaluation Rules}

    \paragraph{Overview.}
    We can now give the evaluation rules of our big-step operational semantics. Figure~\ref{fig:eval-syntax} lists the structure of the
    evaluation rules. Given a statement $\Statement$, the reduction relation gives a transition from a substitution $\Substitution_0$ and
    heap $\Heap_0$ with values for the inputs of $\Statement$, to an heap $\Heap_1$ containing values for the outputs of $\Statement$ and
    an updated substitution $\Substitution_1$. The transition is labeled with a derivation $\Deriv$ which can be considered a trace of the
    steps that were taken in order to make the transition. Similarly, $\DerivsEnv$ contains (at least) a binding for each reference in
    derivation $\Deriv$ and any reference of any derivation in $\DerivsEnv$ itself. There are some variations of this reduction relation
    on the level of statements and rules. An important invariant is that the resulting heap is up to date with respect to the resulting
    substitution.

    The semantics of substitution refinement by defaulting the guesses of a certain scope, starts with an initial substitution
    $\Substitution_0$, and the current scope identifier $\Scope$, and ends in a state $\Substitution_1$. The purpose of this relation is to
    force the evaluation of deferred statements created in $\Scope$ of type $\Type$, such that none of these remain in $\Substitution_1$.
    
    For the evaluation of (monadic) Haskell expressions, we construct an expression $e$ and evaluate it in an execution environment
    $\Haskell$, containing data type definitions, Haskell support code, augmented with bindings for inputs to the expression, including
    the substitution.
    
    \begin{figure}[htp]
    \begin{displaymath}
      \begin{array}[t]{ll}
      \Substitution_0  \attrsep  \Heap_0  \attrsep  \Scope  \attrsep  \Statement  \transition{\DerivsEnv}{\Deriv}  \Heap_1 \attrsep \Substitution_1    & (\mbox{statement reduction}) \\
      \Substitution_0  \attrsep  \Heap_0  \attrsep  \Scope  \attrsep  \Statement_1,\ldots,\Statement_k  \transition{\DerivsEnv}{\Deriv}  \Heap_1 \attrsep \Substitution_1    & (\mbox{statements reduction}) \\
      \Substitution_0  \attrsep  \Heap_0  \attrsep  \Scope  \attrsep  \Rule_{\SchemeName}  \transition{\DerivsEnv}{\Deriv}  \Heap_1 \attrsep \Substitution_1    & (\mbox{rule reduction}) \\
      \Type \attrsep \Substitution_0 \attrsep \Scope \rightarrow^{\DerivsEnv}_{*} \Substitution_1       & (\mbox{scope defaulting}) \\
      \Haskell \vdash e_{\Haskell} \rightarrow w   &  \mbox{(Haskell evaluation)} \\
      \end{array}
    \end{displaymath}
    \caption{Structure of the evaluation rules.}
    \label{fig:eval-syntax}
    \end{figure}

    Given a type inferencer, a triple $(\Schemes,\Rules,\Haskell)$, and an instantiation of scheme $\Scheme$ by means of statement
    $\Statement_{\SchemeName}$ with a heap $\Heap_0$, evaluation of this statement with the inferencer rules is the transition
    \begin{displaymath}
      \emptyset \attrsep \Heap_0 \attrsep 0 \attrsep \Fixate_{*} \: \Statement_{\SchemeName} \transition{\DerivsEnv}{\Deriv}  \Heap_1 \attrsep \Substitution_1
    \end{displaymath}
    according to the smallest reduction relations satisfying the evaluation rules of Figure~\ref{fig:conventional-eval-rules},
    Figure~\ref{fig:apply-eval-rule}, Figure~\ref{fig:nondeterm-eval-rules}, and Figure~\ref{fig:defaulting-eval-rules}. We explain
    these rules in more detail. Furthermore, we assume that the components of the inferencer-triple are available in the rules as a constant.

    \begin{figure}[htp]
    \begin{mathpar}
      \inferrule*[right=Scheme]
        { \Rule_{\SchemeName} \in \Rules \\
          \Heap_{\In'} = \Heap_{\In}(\Bindings_{\In}) \\
          \Heap_{\Out} = \Bindings_{\Out}(\Heap_{\Out'}) \\
          \Substitution \attrsep \Heap_{\In'} \attrsep \Scope \attrsep \Rule_{\SchemeName} \transition{\DerivsEnv}{\Deriv}  \Heap_{\Out'} \attrsep \Substitution' \\
        }
        { \Substitution_1 \attrsep \Heap_{\In} \attrsep \Scope \attrsep \Bindings_{\In} \vdash_\SchemeName \Bindings_{\Out} \transition{\DerivsEnv}{\Deriv}  \Heap_{\Out} \attrsep \Substitution' }

      \inferrule*[right=Unify]
        { \Substitution' = |fst| \; (|run| \; (|unif| \; \Heap(n_1) \; \Heap(n_2)) \; \Substitution \; \Scope) }
        { \Substitution \attrsep \Heap \attrsep \Scope \attrsep n_1 \equiv n_2 \transition{\emptyset}{\diamond} \emptyset \attrsep \Substitution' }
      
      \inferrule*[right=Exec]
        { \Haskell \vdash |run| \; (e \; \Heap(n_1) \ldots \Heap(n_k)) \; \Substitution \; \Scope \rightarrow (\Substitution', (w_1, \ldots, w_l)) \\
          \Heap' = m_1 \mapsto w_1, \ldots m_k \mapsto w_k
        }
        { \Substitution \attrsep \Heap \attrsep \Scope \attrsep \Execution e :: n_1,\ldots,n_k \rightarrow m_1,\ldots,m_l \transition{\emptyset}{\diamond} \Heap' \attrsep \Substitution' }

      \inferrule*[right=Fixstep]
        { \Rule_{\SchemeName} \in \Rules \\
          \Substitution_1 \attrsep \Heap_1 \attrsep \Scope \attrsep \Rule_{\SchemeName} \transition{\DerivsEnv}{\Deriv} \Heap_2 \attrsep \Substitution_2 \\
          \Heap_1(\Idents{n}) \not\eq \Heap_2(\Idents{n}) \\
          \Substitution_2 \attrsep \Heap_2(\Bindings) \; \Heap_2 \attrsep \Scope \attrsep \Fixpoint^{\Idents{n}}_{\Bindings} \; \Statement_{\SchemeName} \transition{\DerivsEnv}{\Deriv'} \Heap_3 \attrsep \Substitution_3 \\
        }
        { \Substitution_1 \attrsep \Heap_1 \attrsep \Scope \attrsep \Fixpoint^{\Idents{n}}_{\Bindings} \; \Statement_{\SchemeName} \transition{\DerivsEnv}{\Deriv \; \Deriv'} \Heap_3 \attrsep \Substitution_3 }

      \inferrule*[right=Fixskip]
        {}
        { \Substitution \attrsep \Heap \attrsep \Scope \attrsep \Fixpoint^{\Idents{n}}_{\Bindings} \; \Statement_{\SchemeName} \transition{\DerivsEnv}{\Deriv} \Heap(\Bindings) \; \Heap \attrsep \Substitution }
    \end{mathpar}
    \caption{Evaluation rules for conventional statements.}
    \label{fig:conventional-eval-rules}
    \end{figure}

    \paragraph{Conventional statements.}
    In Figure~\ref{fig:apply-eval-rule} are the rule for what we call the conventional statements. These are the statements in which type checking algorithms
    can be expressed. Type inference is not possible with these rules yet since this requires guessing.

    The Scheme-rule represents instantiation of a scheme named $\SchemeName$. An inferencer rule $\Rule_{\SchemeName}$ is chosen and evaluated. The bindings
    dictate which values to take from the heap to use as inputs to the rule. Similarly, the bindings dictate under which name the outputs after evaluation
    are to be stored. These inferencer rules must be syntax directed. There should be only one $\Rule_{\SchemeName}$ that can be applied.

    For the unify-rule, the values bound to $n_1$ and $n_2$ are checked for equality with the |unify| function defined on the type of these values. When the
    types involve guesses, this may lead to discovery of more type information about guesses and an updated substitution.
    
    In the exec-rule, the monadic code is executed with the values of $n_1, \ldots, n_k$ as parameter. The monadic code may update the substitution, or
    cause the statement to fail. If the execution succeeds, the returned product of the monadic code contains the values for in the output-heap.
    
    Finally, with the fixpoint-rule a scheme-statement can be repeatedly executed as long as it causes one of the values of $\Idents{n}$ to change. For each repetition, the
    bindings $\Bindings$ dictate which outputs are the inputs of the next iteration. In this case there may be more than one applicable rule
    $\Rule_{\SchemeName}$. However, to make a step, evaluation of the inferencer rule must cause a change of value $n$.

    \begin{figure}[htp]
    \begin{mathpar}
      \inferrule*[right=Rule]
        {
          \Substitution_{\In} \attrsep \Bindings_{\In}(\Heap_{\In}) \attrsep \Scope \attrsep \Statement_1, \ldots, \Statement_k \transition{\DerivsEnv}{\Deriv_1, \ldots, \Deriv_k} \Heap' \attrsep \Substitution_{\Out} \\
          \Deriv = \inferrule{\Deriv_1 \ldots \Deriv_k}{\Heap' \vdash (\Bindings_{\In} \vdash_s \Bindings_{\Out})} \\
          \Heap_{\Out} = \Heap'(\Bindings_{\Out}) \\
        }
        { \Substitution_{\In} \attrsep \Heap_{\In} \attrsep \Scope \attrsep \Statement_1, \ldots, \Statement_k ; \Bindings_{\In} \vdash_s \Bindings_{\Out} \transition{\DerivsEnv}{\Deriv}  \Heap_{\Out} \attrsep \Substitution_{\Out} }

      \inferrule*[right=Statements]
        {
          \Substitution_i \attrsep (\Substitution_i \Heap_i \ldots \Heap_1) \attrsep \Scope \attrsep \Statement_i \transition{\DerivsEnv}{\Deriv_i} \Heap_{i+1} \attrsep \Substitution_{i+1}, \; 1 \leq i \leq k \\
        }
        { \Substitution_1 \attrsep \Heap_1 \attrsep \Scope \attrsep \Statement_1, \ldots, \Statement_k \transition{\DerivsEnv}{\Deriv_1 \ldots \Deriv_k}  \Substitution_{k+1} \Heap_{k+1} \ldots \Heap_1 \attrsep \Substitution_{k+1} }
    \end{mathpar}
    \caption{The apply rules.}
    \label{fig:apply-eval-rule}
    \end{figure}
    
    In Figure~\ref{fig:apply-eval-rule} are evaluation rules for a chosen inferencer rule $\Rule_{\SchemeName}$. The bindings of the conclusion specifies
    under what names the inputs to the rule need to be presented to the statements. Likewise, the bindings also specify under what names the values of the
    outputs of the rule are available. Successful evaluation of a rule means that a derivation $\Deriv$ has been produced, of which the current rule forms
    the root, and the derivations of the premises are its immediate children.

    Evaluation of a sequence of statements causes the heap to accumulate the outputs of the statements already executed
    so far. The outputs of predecessors of a statement in this sequence are also available as input to the statement. Since each evaluation of a statement
    potentially causes more information to be known about guesses in such outputs, the most recent substitution is applied to these predecessor-outputs
    first.
    
    \begin{figure}[htp]
    \begin{mathpar}
      \inferrule*[right=Defer]
        { v, v_1, \ldots, v_l, \RuleIdent \; \mbox{fresh} \\
          \Heap = n \mapsto \sembrack{v}, m_1 \mapsto \sembrack{v_1}, \ldots, m_l \mapsto \sembrack{v_l} \\
          \Heap_{\Out} = \Heap \Heap_{\In} \\
          \Substitution_1 = v_1 \mapsto \bot, \ldots, v_l \mapsto \bot \\
          \Substitution_2 = v \mapsto \Deferred_{\Heap_{\Out}}^{\Scope,\RuleIdent} \; \Statements_1, \ldots, \Statements_k \\
        }
        { \Substitution \attrsep \Heap_{\In} \attrsep \Scope \attrsep \Defer_n^{m_1, \ldots, m_l} \; \Statements_1, \ldots, \Statements_k \transition{\emptyset}{\derivref{\RuleIdent}} \Heap_{\Out} \attrsep \Substitution_1 \Substitution_2 \Substitution }
      
      \inferrule*[right=CommitVar]
        { v \mapsto \Deferred_{\Heap}^{\Scope,\RuleIdent} \; \Statement^{**} \in \Substitution \\
          \Statement_1, \ldots, \Statement_k \in \Statement^{**} \\
          \Substitution_1 = v \mapsto \Heap_{\In}(n), \Substitution_{\In} \\
           \Substitution_1 \attrsep \Substitution_1 \Heap \attrsep \Scope \attrsep \Statement_1, \ldots, \Statement_k \transition{\DerivsEnv}{\Deriv} \Heap' \attrsep \Substitution_2 \\
           \Substitution_{\Out} = \left\{ v \mapsto w \mid n \mapsto \sembrack{v} \in \Heap, n \mapsto w \in \Heap' \right\}, \Substitution_2 \\
           \RuleIdent \mapsto \Deriv \in \DerivsEnv \\
        }
        { \Substitution_{\In} \attrsep \Heap_{\In} \attrsep \Scope \attrsep \Commit_{v_{\Type}} \; n \transition{\DerivsEnv}{\Diamond} \emptyset \attrsep \Substitution_{\Out} }
  
      \inferrule*[right=CommitVal]
        { v \mapsto w \in \Substitution \\
          n' \; \mbox{fresh} \\
          \Heap = n' \mapsto w, n \mapsto \Heap_{\In}(n) \\
          \Substitution_{\In} \attrsep \Heap \attrsep \Scope \attrsep n' \equiv n \transition{\DerivsEnv}{\Diamond} \Heap' \attrsep \Substitution_{\Out}
        }
        { \Substitution_{\In} \attrsep \Heap_{\In} \attrsep \Scope \attrsep \Commit_{v_{\Type}} \; n \transition{\DerivsEnv}{\Diamond} \emptyset \attrsep \Substitution_{\Out} }

      \inferrule*[right=Fixate]
        { \Substitution_{\In} \attrsep \Heap_{\In} \attrsep \Scope \attrsep \Statements \transition{\DerivsEnv}{\Deriv} \Heap_{\Out} \attrsep \Substitution_1 \\
          \Type \attrsep \Substitution_1 \attrsep \Scope \rightarrow^{\DerivsEnv}_{*} \Substitution_{\Out} \\
          \mbox{deferred}(\Scope, \Substitution_{\Out}) = \emptyset \\
        }
        { \Substitution_{\In} \attrsep \Heap_{\In} \attrsep \Scope-1 \attrsep \Fixate_{\Type} \: \Statements \transition{\DerivsEnv}{\Deriv} \Substitution_{\Out} \Heap_{\Out} \attrsep \Substitution_{\Out} }
    \end{mathpar}
    \caption{Evaluation rules for non-determinism annotations.}
    \label{fig:nondeterm-eval-rules}
    \end{figure}

    \paragraph{Non-deterministic statements}
    To deal with guesses, there are the statements that deal with non-determinism, which we will call the non-deterministic statements. Their
    semantics is made precise in Figure~\ref{fig:nondeterm-eval-rules}.
    
    For a Defer-statement, guesses are produced as outputs for $n, m_1, \ldots, m_l$. A closure for the statements $c_1, \ldots, c_k$ is stored as
    substitution for the guess of $n$ as closure. A commit on this guess leads to the execution of these statements, and to the production of
    values for the guesses $m_1, \ldots, m_l$. Committing on these latter guesses is not possible, since the substitution for these guesses is
    mapped to $\bot$. This closure is introduced in scope $\Scope$ and contains a unique identifier $\RuleIdent$ which is the name of the
    derivation that is produced later. A reference to this derivation is returned as the derivation of the Defer-statement.
    
    Evaluation of the Commit-statement leads to the evaluation of the deferred statements. The heap stored in the closure is updated to the
    current substitution, and the substitution reflects the newly found information about the guess. Then, one of the sequences of statements is
    chosen and evaluated. The evaluation has caused outputs to be produced for values that were before represented as a guess to
    a $\bot$-substitution. Subsequently, the substitution is updated such that these substitutions do not map to $\bot$ anymore, but to their
    newly produced value.
    
    Finally, there is the Fixate-statement. Its statement is evaluated in a subscope $\Scope$. After evaluation, the remaining guesses are
    defaulted such that no deferred statement is left for scope $\Scope$. The rules for defaulting are specified in
    Figure~\ref{fig:defaulting-eval-rules}.

    \begin{figure}[htp]
    \begin{mathpar}
      \inferrule*[right=Flex]
        { n, v' \; \mbox{fresh} \\
          v_{\Type} \in \mbox{dom}(\Substitution_{\In}) \\
          \Heap = n \mapsto \sembrack{v'} \\
          \Substitution = v' \mapsto \Deferred_{\emptyset}^{\Scope,\RuleIdent} \; \{ \emptyset \} \\
          \Substitution \Substitution_{\In} \attrsep \Heap \attrsep \Scope \attrsep \Commit_{v_{\Type}} \; n \transition{\DerivsEnv}{\Diamond} \Heap' \attrsep \Substitution_{\Out} \\
        }
        { \Type \attrsep \Substitution_{\In} \attrsep \Scope \rightarrow^{\DerivsEnv}_{*} \Substitution_{\Out} }

      \inferrule*[right=Rigid]
        { n \; \mbox{fresh} \\
          v_{\Type} \in \mbox{dom}(\Substitution_{\In}) \\
          \Heap = n \mapsto \sembrack{v}_F \\
          \Substitution \Substitution_{\In} \attrsep \Heap \attrsep \Scope \attrsep \Commit_{v_{\Type}} \; n \transition{\DerivsEnv}{\Diamond} \Heap' \attrsep \Substitution_{\Out} \\
        }
        { \Type \attrsep \Substitution_{\In} \attrsep \Scope \rightarrow^{\DerivsEnv}_{*} \Substitution_{\Out} }

      \inferrule*[right=Trans]
        { \Type \attrsep \Substitution_{\In} \attrsep \Scope \rightarrow^{\DerivsEnv}_{*} \Substitution \\
          \Type \attrsep \Substitution \attrsep \Scope \rightarrow^{\DerivsEnv}_{*} \Substitution_{\Out} \\
        }
        { \Type \attrsep \Substitution_{\In} \attrsep \Scope \rightarrow^{\DerivsEnv}_{*} \Substitution_{\Out} }
      
      \inferrule*[right=Finish]
        {}
        { \Type \attrsep \Substitution \attrsep \Scope \rightarrow^{\DerivsEnv}_{*} \Substitution }
    \end{mathpar}
    \caption{Defaulting rules.}
    \label{fig:defaulting-eval-rules}
    \end{figure}

    A deferred guess is committed to with either a {\it flexible} guess as value, or with
    a {\it fixed} unknown value. In both cases, of the deferred statement-sequences must be able to evaluate with this newly found information.
    In the first case, such deferred statement observes a guess as value for its input, and is allowed to refine it. In the later case,
    the value may not be touched.

    Since committing to a flexible guess results in the introduction of a guess in the scope, in order to end up with no guesses in the scope in
    the end, each commit to a flexible guess must lead to refinements of guesses. All guesses that are essentially unconstrained will then end up
    with fixed unknowns.

    \begin{figure}[htp]
    \begin{code}
    type I alpha = ErrorT Err (State (Substitution, Scope)) alpha
    run :: I alpha -> Substitution -> Scope -> (Substitution, alpha)
    \end{code}
    \caption{The |run| function.}
    \label{fig:run-function}
    \end{figure}

    \paragraph{Haskell semantics.}    
    The semantics of the monadic functions are defined in terms of Haskell. The type of the monad is
    given in Figure~\ref{fig:run-function}. It is a conventional combination between the error monad and
    the state monad. The |run| function is the interface between the semantic world and the monad world.
    If the monadic evaluation is successful, the premise with |run| succeeds and there has been a
    state transition into the monad and back. If the evaluation results in an |Err|, the premise with
    |run| does not hold.

  \subsection{Data-type Semantics}
  
    The semantics of the previous section makes some assumptions about the types of identifiers occurring
    in de inferencer rules. This functionality needs to be available in terms of instances for the type
    classes listed in Figure~\ref{fig:data-classes}. This functionality does not have to be available for
    all types. Most of this functionality can be generically derived from the structure of the types.

    \begin{figure}[htp]
    \begin{code}
     class Container alpha where
       appSubst :: (forall beta dot Container beta => beta -> beta) -> alpha -> alpha

    class Unifyable alpha where
      unify ::  (forall beta dot Unifyable beta => beta -> beta -> I ())
                -> alpha -> alpha -> I ()

    class Deferrable alpha where
      deferVars        :: alpha -> {GuessVar}
      mkDeferValue     :: GuessVar -> alpha
      mkFixedValue     :: GuessVar -> alpha
      matchDeferValue  :: alpha -> Maybe GuessVar
      matchFixedValue  :: alpha -> Maybe GuessVar
    \end{code}
    \caption{Semantics on data.}
    \label{fig:data-classes}
    \end{figure}

    A |Container| instance is required to be defined for all types containing guesses. Substitution
    application |Substitution ||=> w| replaces all occurrences of guess $\sembrack{v}$ with $w'$, given
    $v \mapsto w'_{\Type} \in \Substitution$. It generically handles the substitution of guesses, and uses
    |appSubst| to traverse the type.

    \begin{figure}[htp]
    \begin{code}
      unif w1 w2 = unif' unif w1 w2
      unifOne w1 w2 = unif' (\_ _ -> return ()) w1 w2
      ^^^
      unif' r w1 w2  = do  w3 <- update w1; w4 <- update w2
                           unif'' r w1 w2
      ^^^
      unif'' _  w1     w2     | w1 == w2               = return ()
      unif'' _  semv1  semv2                           = compose v1 v2
      unif'' _  semv   w      | v notelem deferVars w  = commit v w
                              | otherwise              = fail "occur check"
      unif'' _  w      semv   | v notelem deferVars w  = commit v w
                              | otherwise              = fail "occur check"
      unif'' r  w1     w2                              = unify r w1 w2


    \end{code}
    \caption{Unification and guess specialization.}
    \label{fig:unif-function}
    \end{figure}

    For the type of the identifiers of the unification rule a |Unifyable| instance needs to be defined,
    which asks for a definitely of the |unify| function. Its sole purpose is to succeed if and only if the
    heads of the two inputs are structurally equal. For the rest, it should delegate to its recursion
    parameter. This function does not have to deal with guesses, since those are handled generically by the
    |unif| function (Figure~\ref{fig:unif-function}). 

    The |unif| function deals with guesses by committing concrete type information to the guess,
    unless both values are guesses. In that case, it takes the composition of the two. This means that
    the two guesses are substituted with a single guess, such that when information is committed to
    this single guess (in the minimal scope of the two), the deferred statements of the original guesses
    are sequenced after each other. Since we require confluence with respect to the order information
    about guesses is found, the execution order of these guesses is allowed to be arbitrary. Also note
    that not all |Unifyable| types have to be |Deferrable|, depending on the properties of the type. We
    omitted this detail here, as it is only a minor detail, and the code for |unif| would be
    considerably more complicated.

%% \section{Translation To Haskell}
%% 
%% Attribute Grammar-like mapping, except that the semantics functions instead of a let, is a monad, such that we can use ST-refs
%% to represent the substitutions efficiently.
%% Not sure if it is worth working this out though.


\section{Conclusion}

  Type rules of declarative type systems contain non-deterministic aspects. These aspects are problematic when writing an inference
  algorithm. We presented a domain specific language for inferencers that has special syntax to formulate algorithms to resolve
  these non-deterministic aspects. The main concept is a first-class guess, which acts as a remote control to a deferred derivation.
  By manipulating guesses, the deferred derivations can be scheduled such that decisions are made at the moment sufficient information
  has become available. The result is that we can write inference algorithms by means of annotating the declarative rules of a
  type system, describing the global scheduling locally, without breaking the relative isolation of the type rules, and without
  breaking soundness with respect to the original rules.

  \paragraph{Future Work}
  We intend to formalize the type system of UHC~\cite{DBLP:conf/ifl/DijkstraFS07}, and generate portions of the inference algorithm
  from this description. We made design decisions that the generated algorithm to be reasonably efficient. Although we conceptually
  explained semantics of the language in monadic terms, we actually generate code for multi-pass higher-order attribute grammars.
  An open question is still if we can keep the complexity of some of UHC's efficient data structures hidden from the type rules.

\acks

This work was supported by Microsoft Research through its European PhD Scholarship Programme.
Thanks to the LerNet Alpha-project to have made it possible to carry out the research for
this paper at the Universidade Federal de Minas Gerais in Brazil.
Finally, Arie wants to thank Annette Bieniusa for her extensive proof reading, and the
Universit\:{a}t Freiburg for providing a friendly and hospitable environment to work on
the actual writing of the paper.

\bibliographystyle{plainnat}
\bibliography{references}

\newpage
\appendix
{\it Appendices to be moved to a technical report.}

\section{Static Semantics}
\label{sect:static-semantics}

  In this section we define the static semantics of the type inference language. This semantics expresses
  when a type inferencer $(\Schemes,\Rules,\Haskell)$ is correctly typed. Concretely, this means that all
  identifiers are defined before used, all used schemes are defined, identifiers participating in equality,
  defer and commit statements have the required properties defined for their types, and Haskell fragments have a type
  corresponding to the type of its inputs and outputs. When this is the case, then compiling the inferencer
  rules to an algorithm in Haskell gives a type correct Haskell program, and during the evaluation of the
  inferencer rules according to the operational semantics of Section~\ref{sect:operational-semantics}, the
  values in the heap have the type of the identifiers if this was the case for the initial heap.

    \begin{figure}[htp]
    \begin{displaymath}
    \begin{array}[t]{ll}
      \vdash \Rule_{\SchemeName}                                             & (\mbox{rule judgment}) \\
      \overline{\alpha} \attrsep \Env_{\In} \vdash \Statement : \Env_{\Out}  & (\mbox{statement judgment}) \\
      \Haskell \vdash e : \Type                                              & (\mbox{Haskell judgment}) \\
    \end{array}
    \end{displaymath}
    \caption{Structure of static semantics judgments.}
    \label{fig:static-judgements}
    \end{figure}

  The structure of the typing judgments is given in Figure~\ref{fig:static-judgements}. The rules may refer
  to the set of schemes $\Schemes$, which is assumed as a constant. All schemes are explicitly typed.
  Local identifiers of an inferencer rule have implicit types which are directly related to the explicit types of schemes
  due to bindings, or to a type of a Haskell fragment due to inputs and outputs.
  
  We give type rules for two typing judgments, one for a rule and one for a statement. The other judgments are
  rather trivial and left out. For the validity of schemes we want to remark that the names of identifiers
  in the input environment must be disjoined to those of the output environment, and that all types in the
  schemes must have a correct kind with respect to the types in $\Haskell$.
  About the typing judgment for statements, we remark that it mentions types $\overline{\alpha}$. These are
  the types over which the scheme, rule, and statements are polymorphic. Technically, the types of identifiers
  in the environments may have free variables, but only if their explicitly occur in $\overline{\alpha}$.
  Also, the output environment contains exactly the types for the outputs of the premise, whereas the input
  environment contains at least the types for the identifiers that are input to the premise.

    \begin{figure}[htp]
    \begin{mathpar}
      \inferrule*[right=Rule]
        { \forall \overline{\alpha} . \; \Env_{\In} \vdash_{\SchemeName} \Env_{\Out} \in \Schemes(\SchemeName) \\
          \mbox{dom}(\Env_{\In}) = \mbox{dom}(\Bindings_{\In}) \\
          \mbox{dom}(\Env_{\Out}) = \mbox{dom}(\Bindings_{\Out}) \\
          \overline{\alpha} \attrsep \Env_{\In}(\Bindings_{\In}) \; \Env'_{j < i} \vdash \Statement_i : \Env'_i, \; 1 \leq i \leq k \\
          \Env_{\Out}(\Bindings_{\Out}) \subseteq \Env_{\In}(\Bindings_{\In}) \; \Env'_{j \leq k} \\
        }
        { \vdash \Statement_1, \ldots, \Statement_k ; (\Bindings_{\In} \vdash_{\SchemeName} \Bindings_{\Out}) }
    \end{mathpar}
    \caption{Rule typing rule.}
    \label{fig:static-rule-rules}
    \end{figure}

  To type check an inferencer rule, we check that a scheme has been defined for it, and verify the define-before-use
  requirement on the premises. Outputs of these premises are accumulated, and the next premise in the sequence may
  use any of these outputs. The local identifiers that are connected to the types of the scheme due to bindings, must
  have types that agree with the types of the scheme.

    \begin{figure}[htp]
    \begin{mathpar}
      \inferrule*[right=Scheme]
        { \forall \overline{\beta} . \; \Env'_{\In} \vdash_{\SchemeName} \Env'_{\Out} \in \Schemes(\SchemeName) \\
          [\overline{\beta} \coloneqq \overline{\Type}] \: \Env'_{\In} \subseteq \Env_{\In}(\Bindings_{\In}) \\
        }
        { \overline{\alpha} \attrsep \Env_{\In} \vdash (\Bindings_{\In} \vdash \Bindings_{\Out}) : \Bindings_{\Out}([\overline{\beta} \coloneqq \overline{\Type}] \: \Env'_{\Out}) }
      
      \inferrule*[right=Unify]
        { n_1 ::_{\Prop_1} \Type \in \Env \\
          n_2 ::_{\Prop_2} \Type \in \Env \\
          \Prop_1 \not= \emptyset \\
          \Prop_2 \not= \emptyset \\
        }
        { \overline{\alpha} \attrsep \Env \vdash n_1 \equiv n_2 : \emptyset }

      \inferrule*[right=Execution]
        { \Type_{\In} = \Env_{\In}(n_1) \rightarrow \ldots \rightarrow \Env_{\In}(n_k) \\
          \Type_{\Out} = (\Type_{1,\Prop_1}, \ldots, \Type_{l,\Prop_l}) \\
          \Haskell \vdash e : \forall \overline{\alpha} . \; \Type_{\In} \rightarrow I \; \Type_{\Out} \\
          \Env_{\Out} = m_1 ::_{\Prop_1} \Type_1, \ldots, m_l ::_{\Prop_l} \Type_l \\
        }
        { \overline{\alpha} \attrsep \Env_{\In} \vdash \Execution e :: (n_1 \ldots n_k) \rightarrow (m_1 \ldots m_l) : \Env_{\Out} }

      \inferrule*[right=Fixpoint]
        { \overline{\alpha} \attrsep \Env_{\In} \vdash \Statement_{\SchemeName} : \Env_{\Out} \\
          \Env_{\In}(m_i) = \Env_{\Out}(m'_i), \; 1 \leq i \leq k \\
          \Idents{n} \subseteq \mbox{dom}(\Env_{\In}) \\
          \Idents{n} \subseteq \mbox{dom}(\Env_{\Out}) \\
        }
        { \overline{\alpha} \attrsep \Env_{\In} \vdash \Fixpoint_{m_1 \mapsto m'_1, \ldots, m_k \mapsto m'_k}^{\Idents{n}} \; \Statement_{\SchemeName} : \Env_{\Out} }

      \inferrule*[right=Defer]
        { \Env_{\In} \; \Env'_{j < i} \vdash \Statement_i : \Env'_i, \; 1 \leq i \leq k \\
          \Env_{\Out} = \Env'_{j \leq k} \ \{n,\Idents{m}\} \\
          n ::_{\mbox{d}} \tau \in \Env_{\Out} \\
        }
        { \overline{\alpha} \attrsep \Env_{\In} \vdash \Defer_n^{\Idents{m}} \; \Statements_1, \ldots, \Statements_k : \Env_{\Out} } 

      \inferrule*[right=Commit]
        { n ::_{\mbox{d}} \Type \in \Env_{\In} }
        { \overline{\alpha} \attrsep \Env_{\In} \vdash \Commit_{v_{\Type}} \; n : \emptyset }

      \inferrule*[right=Fixate]
        { \overline{\alpha} \attrsep Env'_{j < i}, \Env_{\In} \vdash \Statement_i : \Env'_i, \;1 \leq i \leq k }
        { \overline{\alpha} \attrsep \Env_{\In} \vdash \Fixate_{\Type} \: \Statement_1, \ldots, \Statement_k : Env'_{j \leq k} }
    \end{mathpar}
    \caption{Statement typing rule.}
    \label{fig:static-statement-rules}
    \end{figure}

  The typing rules for statements are listed in Figure~\ref{fig:static-statement-rules}. The typing judgement states that given some
  types $\overline{\alpha}$, and an environment $\Env_{\In}$ stating which identifiers are in scope and what type and properties these
  have, that the statement produces outputs with types $\Env_{\Out}$. We now focus at some aspects of these rules.
  
  \paragraph{Polymorphism.}
  A limited form of polymorphism is allowed for the types of the inferencer rules, by allowing the types to be parametrized over
  type variables $\overline{\alpha}$. This is useful when in order to be able to reuse some of the inferencer rules, or when the
  syntax of the language we are writing an inferencer for is itself polymorphic (for example, parametrized over the types of
  variables). In fact, we will silently also allow ad-hoc polymorphism by having a set of type class constraints over these variables
  $\overline{\alpha}$, for example to be able to show values of such a polymorphic type, to test for equality, or to use such values
  in maps.
  
  This polymorphism is visible at two places. In the scheme-rule, when instantiating a scheme polymorphic in $\overline{\beta}$, we can
  choose the types for these variables. And in the execution-rule, the type of the monadic function must be polymorphic over the
  variables the scheme itself is polymorphic.

  \paragraph{Haskell.}
  To type monadic functions, we use Haskell's typing relation with an initial environment $\Haskell$ (containing several utility
  functions, data types, etc.). The monadic function is a function of taking some of the inputs of the execution-statement, and
  returning the outputs in monad $I$.
  
  \paragraph{Properties.}
  Some statements can require additional semantics defines on the values they operate on. For example, in order to test two values
  for equality in the unify-rule, we require a |Unify|-instance to be defined on the type. This is encoded in the language as a property
  $\Prop$ of an identifier. There are three properties: none, unifyable, and deferrable. When an identifier has the deferrable
  property, there are instances of both |Unifyable| as |Deferrable| for its type. The commit and defer statements require this deferrable
  property to be defined for the identifiers they act on.


\section{Soundness Almost For Free}
\label{sect:soundness}

  In general, it is hard to prove that a concrete type inference algorithm is sound with respect to the
  type system it is based on. Formally this means that if the inferencer manages to infer a type for
  some program value, that this is indeed a correct type for the program value according to the type
  system. However, when the inference algorithm is described with the inferencer rules, we almost get
  the soundness almost for free.
  
  \paragraph{Almost free.}
  The almost-part is due to some assumptions that we need to make:
  \begin{itemize}
  \item The inferencer rules contains concrete algorithms in the form of Haskell fragments at the
     places where type rules has (non-judgement) premises. For example, when a type rule has a
     premise $\overline{\alpha} \;\#\; \Gamma$, the the type inferencer rule has a premise
     \begin{code}
     let alphas = ftv ty - ftv Gamma
     \end{code}. In the first case, we only state a demand on $\overline{\alpha}$, in the later
     case we precisely define what $\overline{\alpha}$ is. For soundness, we need the guarantee
     that the code fragment ensures that the constraint on $\overline{\alpha}$ is satisfied.
  \item The inferencer rules may be more fine-grained than the type rules in order to deal with
     syntax-directness and explicit scheduling or pipelining of certain rules. For example, 
     for the HML type system, we have a master rule for instantiation with the sole purpose to
     orchestrate the specific rules for instantiation.
     Therefore, we require an erasure function $\erasure{\cdotp} :: \Deriv \rightarrow \Deriv$ that
     eliminates the extra structure from the derivation, such that a derivation is left that
     matches the structure of the type rules.
  \end{itemize}

  \paragraph{Notation.}
  Let $D_T(\SchemeName, \Heap)$ stand for the set of all derivations $\Deriv$ that
  are valid according to type system $T$ for an instantiation of scheme $\SchemeName$ of $T$, 
  with the bindings for the identifiers of the scheme in $\Heap$.
  
  Let $\DerivsEnv(\Deriv)$ stand for the substitution and merging of a derivations reference
  $\derivref{\RuleIdent}$ in $\Deriv$ with the derivations named $\RuleIdent$ in
  $\DerivsEnv$. Let $\DerivsEnv_{*}(\Deriv)$ stand for the fixpoint. If $\DerivsEnv$ is complete,
  then there is no reference left in the result.

  \paragraph{Soundness.}
  
  \begin{thm}
  Suppose that $(\Schemes,\Rules,\Haskell)$ is an inferencer for some type system $T$,
  $\Heap_0$ and $\Heap_1$ are some heap, and $\Statement_{\SchemeName}$ is an instantiation of
  one of the schemes of $T$.
  Now suppose that there is some substitution $\Substitution$ such that:
  \begin{displaymath}
    \emptyset \attrsep \Heap_{\In} \attrsep 0 \attrsep \Fixate \: \Statement_{\SchemeName} \transition{\DerivsEnv}{\Deriv} \Heap_{\Out} \attrsep \Substitution
  \end{displaymath}
  Then:
  \begin{displaymath}
    \erasure{\DerivsEnv_{*}(\Deriv)} \in D_T(\SchemeName, \Heap_{\Out} \Heap_{\In})
  \end{displaymath}
  \end{thm}
  
  \begin{proof}
  We give a sketch. First note that only fixate-statements introduce a scope, and also guarantee that there are no deferred
  statements remaining in this scope. More concretely, $\Substitution$ does not have any deferred statements. Derivations
  references are only introduced by a deferred-statement, which also brings equally named deferred statements in scope.
  Since these have all been resolved, it means that $\DerivsEnv$ is complete, and thus $\DerivsEnv_{*}(\Deriv)$ is a full
  derivation without any references.
  
  By taking the erasure of this derivation, we obtain a derivation $\Deriv'$. In order to show that
  $\Deriv' \in D_T(\SchemeName, \Heap_{\Out} \Heap_{\In})$, we need to prove for each node in $\Deriv'$,
  the bindings in its heap $\Heap$, satisfy the premises of the rule in $T$ corresponding with the node.
  
  If there is no guessing involved, then we know that Haskell fragments have successfully run and ensured that these
  premises did hold. Now, when guessing was involved, this means that a certain order of evaluation was taken in order
  to produce the values. Some values where inspected before the full values where known. We now need to show that the
  same values would be observed when this evaluation would have occurred at the very end of the inference process.

  We use an important property: all values are constant up to the guesses. Once a commit has been done on
  a guess, it is never rolled back. This has as consequence that once we observe that a value has a certain structure, this
  value can be considered to always keep having this structure. Therefore, a Haskell fragment executing too late does not
  matter, but what if it executed too early, and thus saw only a partial value?

  There are two cases to consider. The first case is that enough of the value was known to complete evaluation. These portions of the
  value cannot have changed until the end of the inference process, and thus that evaluation would still be valid at a
  later time. In the second case, not enough of the value was known, and some of the evaluation was deferred. In that case,
  since all deferred statements have executed, this means that the evaluation was done successfully at a later time.
  \end{proof}
  
  \paragraph{Remarks.}
  This section talked about soundness only. Dual to soundness is completeness. Unfortunately, completeness cannot be proved in
  general, because we can encode type systems for which no inference algorithm exists (for example, implicitly typed
  System F). However, we can ask ourselves the question what constraints we need on the type system and the Haskell fragments
  in the inferencer rules, in order to be able to prove completeness. As for now, we do not have an answer to this, otherwise,
  interesting question. Also, we note that one wants to experiment with inference algorithms. Soundness with respect to the
  type system is immediately wanted, but completeness is something we expect only to achieve after playing around sufficiently
  and making the right implementation decisions.

%if False
\section{Future Work}

  \paragraph{Limitations.}
  The inference algorithm has as property that refinements on a guess are never undone.
  This eliminates the need for backtracking, which ensures that a fixed traversal over
  the AST is sufficient to construct the derivation. We believe that this works for many
  kinds of non-deterministic aspects of type rules. On the other hand, there are type
  rules that cannot be mapped to a fixed traversal, but require a constraint-solving
  algorithm (for example, Constraint Handling Rules). For example, type rules related
  to overloading in Haskell. One particular question is if we can discover what
  non-deterministic aspect cannot be dealt with by a single traversal, and map all
  rules that depend on it to CHR constraints.

  \paragraph{Extensions}
  \begin{itemize}
  \item Higher-order abstractions for type rules.
  \item Transformations of certain patterns. Automatic insertion of |fresh| when input is not
    available yet. Automatic insertion of $\equiv$ when putting an input at the place of an
    output
  \end{itemize}

  \paragraph{From interpreter to compiler.}
  The practical intention of our research is to generate the type inferencer code for the
  UHC. This requires compiling the inferencer rules instead of interpreting them.
  Declarative aspects of type rules were a major obstacle, since these prevented a
  straightforward mapping to attribute grammar code. With this problem taken care of,
  constructing the compiler is in our reach. Such a compiler is interesting, because it
  allows the rapid prototyping of type systems and encourages experimentation.

  \paragraph{Efficiency.}
We imposed some constraints, such as
  syntax-directed rules for the inference, and disallowing backtracking, in order to have
  a system that can be implemented in terms of proved conventional and reasonably efficient technology. This, however,
  only applies to the evaluation process and scheduling of the rules. The actual computations
  are performed by Haskell code, referenced by the rules, that manipulates the data
  structures such as environments and types.
  
  Experience with the UHC shows that it is the efficiency of this code that matters in order
  to scale up to typing real-world programs. For example, UHC uses a more complicated
  data structure to represent an environment than a conventional map, in order to make some
  operations cheaper and overall memory consumption lower. As a result, some extra invariants
  need to be maintained which make the use of this data structure more complicated. Since the
  inferencer rules are a higher-level abstraction, we expect that the conventional interface
  can be provided to the rules, while we ensure that the extra work required for these special
  data structures is handled by inserting special annotations at the appropriate places.
%endif

\end{document}

