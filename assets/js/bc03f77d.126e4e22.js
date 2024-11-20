"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[7525],{73233:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>l,contentTitle:()=>s,default:()=>u,frontMatter:()=>a,metadata:()=>t,toc:()=>d});const t=JSON.parse('{"id":"erlang-error-index/w/W0017","title":"W0017 - Undefined Function","description":"Error","source":"@site/docs/erlang-error-index/w/W0017.md","sourceDirName":"erlang-error-index/w","slug":"/erlang-error-index/w/W0017","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0017","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":17,"frontMatter":{"sidebar_position":17},"sidebar":"tutorialSidebar","previous":{"title":"W0016 - Deprecated Function","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0016"},"next":{"title":"W0020 - Unused Include","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0020"}}');var i=r(74848),o=r(28453);const a={sidebar_position:17},s="W0017 - Undefined Function",l={},d=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}];function c(e){const n={a:"a",code:"code",em:"em",h1:"h1",h2:"h2",header:"header",p:"p",pre:"pre",strong:"strong",...(0,o.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.header,{children:(0,i.jsx)(n.h1,{id:"w0017---undefined-function",children:"W0017 - Undefined Function"})}),"\n",(0,i.jsx)(n.h2,{id:"error",children:"Error"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-erlang",children:"  main() ->\n    dep:exists(),\n    dep:not_exists().\n%%  ^^^^^^^^^^^^^^^^ \ud83d\udca1 warning: Function 'dep:not_exists/0' is undefined.\n"})}),"\n",(0,i.jsx)(n.h2,{id:"explanation",children:"Explanation"}),"\n",(0,i.jsx)(n.p,{children:"The warning message indicates that the invoked function cannot be found."}),"\n",(0,i.jsx)(n.p,{children:"The problem could be due to misspelling, to the wrong number of arguments passed to the function, to a recent removal of the target function, to a dependency change or to a misconfiguration of the language server."}),"\n",(0,i.jsxs)(n.p,{children:["To fix the problem you should verify whether the invoked function actually exists and has the correct ",(0,i.jsx)(n.em,{children:"arity"}),". Remember that in Erlang a function is identified by its name ",(0,i.jsx)(n.strong,{children:"and"})," the number of arguments it takes."]}),"\n",(0,i.jsxs)(n.p,{children:["In case of false positives, the ",(0,i.jsxs)(n.a,{href:"/erlang-language-platform/docs/erlang-error-index/#ignoring-diagnostics",children:["standard ",(0,i.jsx)(n.code,{children:"elp:ignore"})," mechanism"]})," should be used. Please report this as a bug should this be the case."]}),"\n",(0,i.jsxs)(n.p,{children:["This diagnostic is limited to fully qualified function calls (i.e. function calls which specify the module name), since local calls to undefined functions are already reported by the Erlang linter (see ",(0,i.jsx)(n.a,{href:"/erlang-language-platform/docs/erlang-error-index/l/L1227",children:"L1227"}),")."]})]})}function u(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(c,{...e})}):c(e)}},28453:(e,n,r)=>{r.d(n,{R:()=>a,x:()=>s});var t=r(96540);const i={},o=t.createContext(i);function a(e){const n=t.useContext(o);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function s(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:a(e.components),t.createElement(o.Provider,{value:n},e.children)}}}]);