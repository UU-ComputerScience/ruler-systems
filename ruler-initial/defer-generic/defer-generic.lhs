\documentclass[preprint,natbib]{sigplanconf}

%include lhs2TeX.fmt
%include polycode.fmt

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

\floatstyle{boxed}
\restylefloat{figure}
\setlength{\fboxsep}{1.4pt}



\nsdefine{ruler-algorithmic-old}

  \newcommand\sep{\textcolor{blue}{\:;\:}}
  \newcommand\arr{\textcolor{blue}{\;\Rightarrow\;}}
  \newcommand\red[4]{{#1} \sep {#2} \arr {#3} \sep {#4}}
  \newcommand\subst[2]{{#1} \coloneqq {#2}}
  \newcommand\happend[3]{#1 \mapsto #2, #3}
  \newcommand\hdel[2]{#1 \backslash #2}
  
  \newcommand\freshrel[1]{ #1 \: \mathsf{fresh} }
  
  \newcommand\alloc[2]{ \mathsf{alloc} \; #1 \; #2}
  \newcommand\match[3]{ \mathsf{match} \; #1 \; #2 \; #3 }
  \newcommand\mark[2]{ \mathsf{mark} \; #1 \; #2 }
  
  \newcommand\let[3]{ \mathbf{let} \: #1 = #2 \:;\; #3 }
  \newcommand\heap{\mathcal{H}}
  \newcommand\fresh{\mathbf{fresh}}
  \newcommand\void{\mathsf{void}}

\nsdefine{ruler-algorithmic-names}
  \newcommand\new{\mathsf{new}}
  \newcommand\inst{\mathsf{inst}}
  \newcommand\as{\mathsf{as}}
  \newcommand\fresh{\mathsf{fresh}}
  \newcommand\external{\mathsf{external}}
  \newcommand\derivation{\mathsf{derivation}}
  \newcommand\merge{\mathsf{merge}}
  \newcommand\der{\mathsf{der}}
  \newcommand\clos{\mathsf{clos}}
  \newcommand\pap{\mathsf{pap}}
  \newcommand\attrInput{\mathsf{input}}
  \newcommand\attrOutput{\mathsf{output}}
  \newcommand\first{\mathsf{first}}
  \newcommand\establish{\mathsf{establish}}

\nsdefine{ruler-algorithmic-constructs}
  \nsimport[n]{ruler-algorithmic-names}
  \newcommand\new[1]{\n:new \; #1}
  \newcommand\inst[2]{\n:inst \; #1 \; \n:as \: #2}
  \newcommand\fresh[1]{#1 \: \n:fresh}
  \newcommand\external[2]{\n:external \: #1 \; #2}
  \newcommand\derivation[3]{\n:derivation^{#1} \; #2 \; #3}
  \newcommand\merge[1]{\n:merge \; #1}
  \newcommand\derClosure[3]{\n:clos^{#1} \; #2 \; #3}
  \newcommand\derPAP[4]{\n:pap^{#1} \; #2 \; #3 \; #4}
  \newcommand\derEstablished[3]{\n:der \: #1 \; #2}
  \newcommand\establish[1]{\n:establish \; #1}
  \newcommand\alt[3]{#1 \; #2: \: #3}
  \newcommand\calt[2]{#1: #2}
  \newcommand\attrInput[1]{\n:attrInput \: #1}
  \newcommand\attrOutput[1]{\n:attrOutput \: #1}
  \newcommand\lab{\ell}
  \newcommand\heap{\mathcal{H}}
  \newcommand\sep{\textcolor{blue}{\:;\:}}
  \newcommand\redarrr{\textcolor{blue}{\;\Rightarrow\;}}
  \newcommand\redarrd{\textcolor{blue}{\;\Downarrow\;}}
  \newcommand\redarrt{\textcolor{blue}{\;\triangleright\;}}
  \newcommand\reddef[5]{{#1} \sep {#2} \sep {#3} \redarrr {#4} \sep {#5}}
  \newcommand\reddefs[5]{{#1} \sep {#2} \sep {#3} \redarrr {#4} \sep {#5}}
  \newcommand\redexpr[5]{{#1} \sep {#2} \sep {#3} \redarrd {#4} \sep {#5}}
  \newcommand\redpat[4]{{#1} \sep {#2} \sep {#3} \redarrt {#4}}
  \newcommand\redsel[4]{{#1} \sep {#2} \sep\; {#3} \;\redarrt\; {#4}}
  \newcommand\redacc[3]{{#1} \sep {#2} \sep Acc \; {#3}}
  \newcommand\subst[2]{{#1} \coloneqq {#2}}
  \newcommand\first[1]{\n:first \: \left[\; #1 \;\right]}
  \newcommand\singleton[1]{\{#1\}}
  \newcommand\hsingle[2]{\singleton{#1 \mapsto #2}}
  \newcommand\hconcat[2]{#1 #2}
  \newcommand\happend[3]{#1 \mapsto #2, #3}
  \newcommand\evalorder[2]{#1 \ll #2}

\begin{document}

\title{Semantics for ruler languages}

\authorinfo{Arie Middelkoop}
           {Universiteit Utrecht}
           {ariem@@cs.uu.nl}

\section{Algorithmic Type Rules}
  \nsimport{ruler-algorithmic-constructs}
  \nsimport[n]{ruler-algorithmic-names}
  
   %{ 
      %format dot = "."
      %format dots = "\ldots"
      %format ^ = " "
      %format <<$ = "\left\llbracket "
      %format $>> = " \right\rrbracket"
      %format lamdot = " . \:"
      %format derivation = "\n:derivation"
      %format establish = "\n:establish"
      %format inst = "\n:inst\:"
      %format new = "\n:new\:"
      %format as = "\:\n:as\:"
      %format input = "\:\n:attrInput"
      %format output = "\:\n:attrOutput"
      %format equiv = "\equiv"
      %format fresh = "\n:fresh"
      %format ^^^ = "\;"
      %format external = "\n:external"
      %format primop = "\oplus"

  In this section we give an interpretation of algorithmic type rules. The
  language of algrithmic type rules is a subclass of the type rule language.
  This language has two characteristics. The judgements are functions, as well
  as the statements of their rules. Each meta variable is defined before used.
  Interpretation of these rules gives an inference algorithm. This inference
  algorithm is sound with respect to the type rules, but gives no guarantees
  concerning completeness. This section is therefore a basis for subsequent
  languages, and indeed, completeness plays a substantial role later.
  
  \begin{figure}[htp]
    \begin{displaymath}
      \begin{array}[t]{llcll}
        \mbox{Identifiers}  & x, f &        &                                   & \\
        \mbox{Label}        & l &           &                                   & \\
        \mbox{Values}       & i &           &                                   & \mbox{Primitive value} \\
        \mbox{Statements}   & d & \Coloneqq & p = e                             & \mbox{Binding} \\
                            &   & \mid      & \establish{f}                     & \mbox{Derive derivation} \\
                            &   & \mid      & \fresh{x}                         & \mbox{Fresh value} \\
                            &   & \mid      & e_1 \equiv e_2                    & \mbox{Equivalence} \\
                            & \hat{d} & \Coloneqq & d_{\heap}^{\overline{a}}    & \mbox{Stmt. with context} \\
        \mbox{Expressions}  & e & \Coloneqq & x \;\mid\; f.x \;\mid\; i         & \mbox{Atom} \\
                            &   & \mid      & C \; \overline{e}                 & \mbox{Constructor (sat.)} \\
                            &   & \mid      & \overline{d^\lab}; e              & \mbox{Sequence} \\
                            &   & \mid      & \new{e}                           & \mbox{New derivation} \\
                            &   & \mid      & \derivation{l}{\overline{a}}{\overline{c}} & \mbox{Derivation} \\
                            &   & \mid      & \external{x}{\overline{a}}        & \mbox{External} \\
                            &   & \mid      & \merge{\overline{e}}              & \mbox{Merge derivations} \\
        \mbox{Alternatives} & c & \Coloneqq & \alt{x}{\overline{a}}{\overline{d^\lab}} & \mbox{Named alternative} \\
                            & \hat{c} & \Coloneqq & \calt{x}{\overline{\hat{d}^\lab}} & \mbox{Alt. with context} \\
        \mbox{Patterns}     & p & \Coloneqq & x \;\mid\; f.x \;\mid\; C \: \overline{x}      &  \\
        \mbox{Attributes}   & a & \Coloneqq & \attrInput{x}                     & \mbox{Input attribute} \\
                            &   & \mid      & \attrOutput{x}                    & \mbox{Output attribute} \\
        \mbox{Bindings}     & \heap  & \Coloneqq & \overline{x \mapsto v}       & \\
        \mbox{Objects}      & v & \Coloneqq & i                                 & \mbox{Boxed value} \\
                            &   & \mid      & C \; \overline{v}                 & \mbox{Constructor (sat.)} \\
                            &   & \mid      & \derClosure{\lab}{\overline{a}}{\overline{\hat{c}}} & \mbox{Closure} \\
                            &   & \mid      & \derPAP{\lab}{\overline{a}}{\heap_1}{\overline{\hat{c}}}{x}    & \mbox{Partial application} \\
                            
%                            &   & \mid      & \derPAP{\ell}{\overline{a}}{\heap}{\overline{\hat{c}}}        & \mbox{Partial application} \\
%                            &   & \mid      & \derEstablished{x}{\heap}{\overline{v}}        & \mbox{Established} \\
      \end{array}
    \end{displaymath}
  
    \caption{Ruler-algorithmic syntax}
    \label{fig:ruler-algo-syntax}
  \end{figure}
  

  
  \begin{figure}[htp]
    Statement evaluation ($\reddef{\overline{\lab}}{\heap_1}{d}{\heap_2}{\overline{v}}$):
    \begin{mathpar}
      \inferrule*[right=var]
        { \redexpr{\overline{\lab}}{\heap}{e}{v}{\overline{v}} \\\\
          \heap(x) \equiv v \\
        }
        { \reddef{\overline{\lab}}{\heap}{x = e}{\emptyset}{\overline{v}} }

      \inferrule*[right=field]
        { \derPAP{\ell}{\overline{a}}{\heap_p}{\overline{\hat{c}}} = \heap(f) \\\\
          \redexpr{\overline{\lab}}{\heap}{e}{v}{\overline{v}} \\\\
          \heap_p(x) \equiv v \\
        }
        { \reddef{\overline{\lab}}{\heap}{f.x = e}{\emptyset}{\overline{v}} }

      \inferrule*[right=con]
        { \redexpr{\overline{\lab}}{\heap}{e}{C \: \overline{v}_1}{\overline{v}_2} \\\\
          \overline{x \equiv v_1} \\
        }
        { \reddef{\overline{\lab}}{\heap}{C \: \overline{x} = e}{\emptyset}{\overline{v}_2} }

      \inferrule*[right=fresh]
        { \fresh{i} \\
          v = |mkguess|\;i\\\\
          \heap(x) \equiv v \\
        }
        { \reddef{\overline{\lab}}{\heap}{\fresh{x}}{\emptyset}{\emptyset} }
      
      \inferrule*[right=equiv]
        { \redexpr{\overline{\lab}}{\heap}{e_1}{v_1}{\overline{v}_1} \\
          \redexpr{\overline{\lab}}{\heap}{e_2}{v_2}{\overline{v}_2} \\
          |unif| \; v_1 \; v_2
        }
        { \reddef{\overline{\lab}}{\heap}{e_1 \equiv e_2}{\emptyset}{\overline{v}_1 } \overline{v}_2}

      \inferrule*[right=establish]
        { \derPAP{\lab_1}{\overline{a}}{\heap_p}{\overline{\hat{c}}} = f \\
          \redsel{\lab_1,\overline{\lab}}{\heap_p}{\overline{\hat{c}}}{\calt{x}{\overline{\hat{d}^{\lab_2}}}} \\
          \reddefs{\lab_1,\overline{\lab}}{\heap_p}{\overline{\hat{d}^{\lab_2}}}{\heap_r}{\overline{v}} \\
          v = \derEstablished{x}{(\heap_r \heap_p \mid \overline{a})}{\overline{v}} \\
        }
        { \reddef{\overline{\lab}}{\heap}{\establish{f}}{\hsingle{f}{v}}{v, \overline{v}} }
    \end{mathpar}
    
    Statements evaluation ($\reddefs{\overline{\lab}}{\heap_1}{\overline{\hat{d}^{\lab}}}{\heap_2}{\overline{v}}$):
    \begin{mathpar}
      \inferrule*[right=sequence]
        { \evalorder{\overline{\hat{d}^\lab}}{\overline{d_{\heap_d}^{\overline{a}}}} \\
          \reddef{\overline{\lab}}{((\heap_{i-1} \mid \overline{a}_{i-1}) \ldots (\heap_1 \mid \overline{a}_1) \; \heap_0 \mid \overline{a}_i) \heap_{di}}{d_i}{\heap_i}{\overline{v_i}} \\
        }
        { \reddefs{\overline{\lab}}{\heap_0}{\overline{\hat{d}^\lab}}{(\heap_n \mid \overline{a}_n) \ldots (\heap_1 \mid \overline{a}_1)}{\overline{v_1} \ldots \overline{v_n}} }
    \end{mathpar}

    Selection of alternatives ($\redsel{\overline{\lab}}{\heap}{\overline{\hat{c}}}{\hat{c}}$):
    \begin{mathpar}
      \inferrule*[right=take]
        { \redacc{\overline{\lab}}{\heap}{\overline{\hat{d}^\lab}} }
        { \redsel{\overline{\lab}}{\heap}{\calt{x}{\overline{\hat{d}^\lab}}, \; \overline{c}}{\calt{x}{\overline{\hat{d}^\lab}}} }

      \inferrule*[right=skip]
        { \redsel{\overline{\lab}}{\heap}{\overline{c}}{c_1} }
        { \redsel{\overline{\lab}}{\heap}{c_0, \; \overline{c}}{c_1} }
    \end{mathpar}

    Acceptance of non-recursive head ($\redacc{\overline{\lab}}{\heap}{\overline{\hat{d}}}$):
    \begin{mathpar}
      \inferrule*[right=accept]
        { (\overline{\hat{d}^{\lab_2}_1},\overline{\hat{d}^{\lab_2}_2}) = |break isEstablish| \; \overline{\hat{d}^{\lab_2}} \\
          \reddefs{\overline{\lab}}{\heap_1}{\overline{\hat{d}^{\lab_2}_1}}{\heap_2}{\overline{v}_1} \\
          ((\establish{f})_{\heap_d}^{\overline{a_1} \lab_3},\overline{\hat{d}^{\lab_2}_3}) = \overline{\hat{d}^{\lab_2}_2} \\
          \derPAP{\lab_1}{\overline{a_2}}{\heap_p}{\overline{\hat{c}}} = (\heap_2 \heap_1)(f) \\
          \lab_1 \in \overline{\lab} \\
        }
        { \redacc{\overline{\lab}}{\heap_1}{\overline{\hat{d}^{\lab_2}}} }

      \inferrule*[right=ignore]
        { (\overline{\hat{d}^{\lab}_1},\overline{\hat{d}^{\lab}_2}) = |break isEstablish| \; \overline{\hat{d}^{\lab}} \\\\
          \reddefs{\overline{\lab}}{\heap_1}{\overline{\hat{d}^{\lab}_1}}{\heap_2}{\overline{v}_1} \\\\
          \redacc{\overline{\lab}}{\heap_2}{\overline{\hat{d}^{\lab}_2}}
        }
        { \redacc{\overline{\lab}}{\heap_1}{\overline{\hat{d}^{\lab_2}}} }

      \inferrule*[right=tail]
        { \reddefs{\overline{\lab}}{\heap_1}{\overline{\hat{d}^\lab}}{\heap_2}{\overline{v}} }
        { \redacc{\overline{\lab}}{\heap_1}{\overline{\hat{d}^{\lab}}} }
    \end{mathpar}

    %{
       %format v1
       %format v2
       %format heap = "\heap"
       %format mapsto = "\mapsto"
       %format ass = "\overline{a}"
       %format esta = "(\establish{f})_{\heap}^{\overline{a} \lab}"
    Auxilery relations:
    \begin{code}
      unif v1 v2  | isGuess v1 || isGuess v2  = v1 equiv v2
                  | otherwise                 = unify unif v1 v2
      ^^^
      isEstablish (esta)  = True   ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^  nm (input   x) = x
      isEstablish _       = False  ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^  nm (output  x) = x
      ^^^
      heap | ass = { x mapsto v `elem` heap | x `elem` map nm ass }
    \end{code}
    %}

    \caption{Ruler-algorithmic statement semantics}
    \label{fig:ruler-algorithmic-statements-semantics}
  \end{figure}

  \begin{figure}[htp]
    Expression reduction ($\redexpr{\overline{\lab}}{\heap}{e}{v}{\overline{v}}$):
    \begin{mathpar}
      \inferrule*[right=var]
        {}
        { \redexpr{\overline{\lab}}{\heap}{x}{\heap(x)}{\emptyset} }

      \inferrule*[right=lit]
        {}
        { \redexpr{\overline{\lab}}{\heap}{l}{l}{\emptyset} }

      \inferrule*[right=fld]
        { \derEstablished{n}{\heap_d}{\overline{v}} = \heap(f) }
        { \redexpr{\overline{\lab}}{\heap}{f.x}{\heap_d(x)}{\emptyset} }

      \inferrule*[right=con]
        { \redexpr{\overline{\lab}}{\heap}{e_i}{v_i}{\overline{v}_i} }
        { \redexpr{\overline{\lab}}{\heap}{C \; \overline{e}}{C \; \overline{v}}{\overline{\overline{v}}} }

      \inferrule*[right=nw]
        { \redexpr{\overline{\lab}}{\heap}{e}{\derClosure{\lab}{\overline{a}}{\overline{\hat{c}}}}{\overline{v}} \\\\
          \heap_p = |bindings|\;\overline{a} \\
        }
        { \redexpr{\overline{\lab}}{\heap}{\new{e}}{\derPAP{\lab}{\overline{a}}{\heap_p}{\overline{\hat{c}}}}{\overline{v}} }

      \inferrule*[right=sq]
        { \heap_0 = (|bindings|\;\overline{d}) \; \heap \\
          \reddef{\overline{\lab}}{\heap_{i-1} \ldots \heap_0}{d_i}{\heap_i}{\overline{v}_i} \\
          \redexpr{\overline{\lab}}{\heap_n \ldots \heap_0}{e}{v}{\overline{v}} \\
        }
        { \redexpr{\overline{\lab}}{\heap}{\overline{d^\lab}; e}{v}{\overline{v}_n \ldots \overline{v}_1 \overline{v}} }
      
      \inferrule*[right=closure]
        { \hat{c}_i = |wrapC|\;\heap\;\overline{a}\;c_i }
        { \redexpr{\overline{\lab}}{\heap}{\derivation{\lab}{\overline{a}}{\overline{c}}}{\derClosure{\lab}{\overline{a}}{\overline{\hat{c}}}}{\emptyset} }
    \end{mathpar}
    
    Auxilery relations:
    %{
      %format dss = "\overline{d}"
    \begin{code}
      bindings xs = { x -> v | x `elem` outputs xs, fresh v }
    \end{code}
    %}

    \caption{Ruler-algorithmic expression semantics}
    \label{fig:ruler-algorithmic-expression-semantics}
  \end{figure}

%if False
  \begin{figure}
    %{
       %format v1
       %format v2
       %format heap = "\heap"
       %format heap1
       %format heap2
       %format mapsto = "\mapsto"
  \begin{code}
      unif v1 v2  | isGuess v1 || isGuess v2  = v1 equiv v2
                  | otherwise                 = unify unif v1 v2
      ^^^
      nm (input    x) = x     ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^  isEstablish (establish x) = True
      nm (output   x) = x                                      isEstablish _             = False
      ^^^
      heap1 | heap2 = [ x mapsto v `elem` heap1 | x `elem` dom heap2 ]
    \end{code}
    %}

    \caption{Ruler-algorithmic support code}
    \label{fig:ruler-algorithmic-support-code}
  \end{figure}


  %{  
      %format match = "\overline{\Varid{p} \rightarrow \Varid{e}}"
      %format ei = "\Varid{e}_\Varid{i}"
      %format pi = "\Varid{p}_\Varid{i}"
      %format ewrapi = "\Varid{ewrap}_\Varid{i}"
      %format e1 = "\Varid{e}_\Varid{1}"
      %format en = "\Varid{e}_\Varid{n}"
      %format ewrap1 = "\Varid{ewrap}_\Varid{1}"
      %format ewrapn = "\Varid{ewrap}_\Varid{n}"
      %format ees = "\overline{\Varid{e}}"
      %format rrs = "\overline{\Varid{r}}"
      %format xxs = "\overline{\Varid{x}}"
      %format x1  = "\Varid{x}_\Varid{1}"
      %format xn  = "\Varid{x}_\Varid{n}"
  
  \begin{figure}[htp]
    \begin{code}
      -- |plus = \x y lamdot (+) x y|
      plus = derivation input x output res
        lamx: ^^^  res =  derivation input y output res
                            lamy: ^^^  p = new (external (+)  input x y
                                                              output res)
                                       p^dot^x = x
                                       p^dot^y = y
                                       establish p
                                       res = p^dot^res

      -- |head = \(Cons x xs) lamdot x|
      head = derivation input arg output res
        head: ^^^  (Cons x xs)  = arg
                   res          = x

      -- |length Nil = 0; length (Cons x xs) = 1 + length xs|
      length = derivation input list output n
        nil: ^^^   Nil = list
                   n = 0
        cons: ^^^  (Cons x xs)   = list
                   rec = new length
                   rec^dot^list  = xs
                   p = new plus
                   p^dot^x   = 1
                   establish p
                   p' = new p^dot^res
                   p'^dot^y  = rec^dot^n
                   establish p'
                   n         = p'^dot^res
    \end{code}

    \caption{Ruler-algorithmic examples lambda calculus}
    \label{fig:ruler-algorithmic-examples-lambda}
  \end{figure}
  
  \begin{figure}[htp]
    \begin{code}
      <<$ x $>>            =  derivation output res
                                var: ^^^ res = x

      <<$ l $>>            =  derivation output res
                                lit: ^^^ res = l

      <<$ C ees $>>        =  derivation output res
                                con: ^^^  ewrap1 = new <<$ e1 $>> ; establish ewrap1
                                          dots
                                          ewrapn = new <<$ en $>> ; establish ewrapn
                                          res = C ewrap1^dot^res dots ewrapn^dot^res

      <<$ primop ees $>>   =  derivation output res
                                prim: ^^^  ewrap1 = new <<$ e1 $>> ; establish ewrap1
                                           dots
                                           ewrapn = new <<$ en $>> ; establish ewrapn
                                           primcall = new (external primop
                                                             input xxs output res)
                                           primcall^dot^x1  = ewrap1^dot^res
                                           dots
                                           primcall^dot^xn  = ewrapn^dot^res
                                           establish primcall
                                           res              = primcall^dot^res

      <<$ f e $>>          =  derivation output res
                                appwrap: ^^^  fwrap = new <<$ f $>> ; establish fwrap
                                              ewrap = new <<$ e $>> ; establish ewrap
                                              fcall = new fwrap^dot^res
                                              fcall^dot^arg  = ewrap^dot^res
                                              establish fcall
                                              res            = fcall^dot^res
      
      <<$ \x lamdot e $>>  =  derivation output res
                                abswrap: ^^^ res  =  derivation   input arg
                                                                  output res
                                                       abs: ^^^  x = arg
                                                                 ewrap = new <<$ e $>>
                                                                 establish ewrap
                                                                 res  = ewrap^dot^res
      
      <<$ let x = e in b $>>
                           =  derivation output res
                                letwrap: ^^^  x fresh
                                              ewrap = new <<$ e $>> ; establish ewrap
                                              x equiv ewrap^dot^res
                                              bwrap = new <<$ b $>> ; establish bwrap
                                              res = bwrap^dot^res
      
      <<$ case s of match $>>
                           =  derivation output res
                                scrut: ^^^  swrap = new <<$ s $>> ; establish swrap
                                            rwrap = new (derivation output res
                                                           dots
                                                           i: ^^^  pi = swrap^dot^res
                                                                   ewrapi = new <<$ ei $>>
                                                                   establish ewrapi
                                                                   res = ewrapi^dot^res
                                                           dots
                                                        ) ; establish rwrap
                                            res = rwrap^dot^res
    \end{code}
    
    \caption{Translation from lambda calculus to ruler-algorithmic}
    \label{fig:lambda-translation}
  \end{figure}
  %}


  \begin{figure}[htp]
   %{ 
      %format xinps = "\overline{\Varid{x}_{\mbox{inp}}}"
      %format xouts = "\overline{\Varid{x}_{\mbox{out}}}"
      %format vdash = "\vdash"
      %format ri = "\Varid{r}_\Varid{i}"
      %format ruledecl = "\inferrule{\Varid{s}_\Varid{1} \ldots \Varid{s}_\Varid{n}}{\Varid{c}}"
      %format s1 = "\Varid{s}_\Varid{1}"
      %format sn = "\Varid{s}_\Varid{n}"
      %format rrc = $>> "^" c
      %format rrp1 = $>> "^" p "_" 1
      %format rrpn = $>> "^" p "_" n
      %format rrpi = $>> "^" p "_" i
      %format pinps = "\overline{\Varid{p}}"
      %format eouts = "\overline{\Varid{e}}"
      %format einps = "\overline{\Varid{e}}"
      %format pouts = "\overline{\Varid{p}}"
      %format subi = "_{\!\Varid{i}}"
      %format eouti = "\Varid{e}_\Varid{i}"
      %format pinpi = "\Varid{p}_\Varid{i}"
      %format pouti = "\Varid{p}_\Varid{i}"
      %format poutj = "\Varid{p}_\Varid{j}"
      %format einpi = "\Varid{e}_\Varid{i}"
      %format einpj = "\Varid{e}_\Varid{j}"
      %format xinpi = "\Varid{x}_{\mbox{inp},\Varid{i}}"
      %format xinpj = "\Varid{x}_{\mbox{inp},\Varid{j}}"
      %format xoutpi = "\Varid{x}_{\mbox{out},\Varid{i}}"
      %format xoutpj = "\Varid{x}_{\mbox{out},\Varid{j}}"
      %format calli = "\Varid{call}_\Varid{i}"
      %format e1calli = "\Varid{e}_1\Varid{call}_\Varid{i}"
      %format e2calli = "\Varid{e}_2\Varid{call}_\Varid{i}"
      %format e1
      %format e2
  
    \begin{code}
      <<$ x: xinps vdash xouts ; rrs $>>
                                     =  ^^^ ^^^  x =  derivation  input xinps
                                                                  output xouts
                                                        dots <<$ ri $>> dots

      <<$ ruledecl x $>>             =           x: ^^^  <<$ s1 rrp1 ^^^;^^^ dots ^^^;^^^ <<$ sn rrpn ^^^;^^^ <<$ c rrc

      <<$ x fresh rrpi               =           x fresh

      <<$ e1 equiv e2 rrpi           =           e1calli = new <<$ e1 $>> ; establish e1calli
                                                 e2calli = new <<$ e2 $>> ; establish e2calli
                                                 e1calli^dot^res equiv e2calli^dot^res

      <<$ x: einps vdash pouts rrpi  =           calli = new x
                                                 dots calli^dot^xinpj  = einpj ^^^;^^^ dots
                                                 establish calli
                                                 dots poutj            = calli^dot^xoutpj ^^^;^^^ dots

      <<$ x: pinps vdash eouts rrc   =           dots  pinpi = xinpi dots
                                                 dots  ewrapi = new <<$ eouti $>>
                                                       establish ewrapi
                                                       xoutpi = ewrapi^dot^res ^^^ dots
    \end{code}
   %} 

    \caption{Translation from rules to ruler-algorithmic}
    \label{fig:rules-translation}
  \end{figure}
  
  \begin{figure}[p]
    \begin{code}
      lookup  =  external primLookup input env x output ty
      extend  =  external primExtend input x ty env output env'
      
      tpExpr  =  derivation input env e output ty
                   var:  ^^^  (EVar x)         = e
                              dLookup = new lookup
                              dLookup^dot^env  = env
                              dLookup^dot^x    = x
                              establish dLookup
                              ty               = dLookup^dot^ty
                   app:       (EApp f a)       = e
                              df = new tpExpr
                              da = new tpExpr
                              df^dot^e         = f
                              da^dot^a         = a
                              df^dot^env       = env
                              da^dot^env       = env
                              establish df
                              establish da
                              tyr fresh
                              df^dot^ty equiv TArr da^dot^ty tyr
                   lam:       (ELam x e)       = e
                              tyx fresh
                              dExtend = new extend
                              dExtend^dot^x    = x
                              dExtend^dot^ty   = tyx
                              dExtend^dot^env  = env
                              establish dExtend
                              de = new tpExpr
                              de^dot^e         = e
                              de^dot^env       = dExtend^dot^env'
                              establish de
                              ty               = TArr tyx de^dot^ty
    \end{code}

    \caption{Ruler-algorithmic example monomorphic type system}
    \label{fig:ruler-algorithmic-examples-rules}
  \end{figure}
  
  Need variable convention to prevent unwanted name capture and recursion.

%}

%endif

\end{document}

