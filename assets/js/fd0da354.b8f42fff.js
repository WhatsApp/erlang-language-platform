"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[8858],{57154:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>d,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>t,toc:()=>l});const t=JSON.parse('{"id":"erlang-error-index/w/W0009","title":"W0009 - Redundant Assignment","description":"Error","source":"@site/docs/erlang-error-index/w/W0009.md","sourceDirName":"erlang-error-index/w","slug":"/erlang-error-index/w/W0009","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0009","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":9,"frontMatter":{"sidebar_position":9},"sidebar":"tutorialSidebar","previous":{"title":"W0008 - Unreachable Test","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0008"},"next":{"title":"W0010 - Unused Function Argument","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0010"}}');var s=r(74848),a=r(28453);const o={sidebar_position:9},i="W0009 - Redundant Assignment",d={},l=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}];function c(e){const n={code:"code",h1:"h1",h2:"h2",header:"header",p:"p",pre:"pre",...(0,a.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.header,{children:(0,s.jsx)(n.h1,{id:"w0009---redundant-assignment",children:"W0009 - Redundant Assignment"})}),"\n",(0,s.jsx)(n.h2,{id:"error",children:"Error"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-erlang",children:"do() ->\n    X = 42,\n    Y = X,\n %% ^^^^^ assignment is redundant\n    foo(Y).\n"})}),"\n",(0,s.jsx)(n.h2,{id:"explanation",children:"Explanation"}),"\n",(0,s.jsxs)(n.p,{children:["The error message is indicating that the assignment ",(0,s.jsx)(n.code,{children:"Y = X"})," is un-necessary. The variable ",(0,s.jsx)(n.code,{children:"Y"})," is unbound during the assignment and the value resulting from the assignment is then passed to the function ",(0,s.jsx)(n.code,{children:"foo/1"}),"."]}),"\n",(0,s.jsx)(n.p,{children:"A more concise way to express the above would be:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-erlang",children:"do() ->\n    X = 42,\n    foo(X).\n"})})]})}function u(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(c,{...e})}):c(e)}},28453:(e,n,r)=>{r.d(n,{R:()=>o,x:()=>i});var t=r(96540);const s={},a=t.createContext(s);function o(e){const n=t.useContext(a);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function i(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:o(e.components),t.createElement(a.Provider,{value:n},e.children)}}}]);