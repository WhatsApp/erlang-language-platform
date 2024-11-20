"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[2963],{19002:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>i,contentTitle:()=>s,default:()=>u,frontMatter:()=>c,metadata:()=>o,toc:()=>l});const o=JSON.parse('{"id":"erlang-error-index/o/O0006","title":"O0006 -    Quote ended unexpectedly","description":"Error","source":"@site/docs/erlang-error-index/o/O0006.md","sourceDirName":"erlang-error-index/o","slug":"/erlang-error-index/o/O0006","permalink":"/erlang-language-platform/docs/erlang-error-index/o/O0006","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":6,"frontMatter":{"sidebar_position":6},"sidebar":"tutorialSidebar","previous":{"title":"O0005 - ` Quote ended unexpectedly","permalink":"/erlang-language-platform/docs/erlang-error-index/o/O0005"},"next":{"title":"O0007 -   ` Quote ended unexpectedly","permalink":"/erlang-language-platform/docs/erlang-error-index/o/O0007"}}');var t=r(74848),a=r(28453);const c={sidebar_position:6},s="O0006 - `` Quote ended unexpectedly",i={},l=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}];function d(e){const n={a:"a",code:"code",em:"em",h1:"h1",h2:"h2",header:"header",p:"p",pre:"pre",...(0,a.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.header,{children:(0,t.jsxs)(n.h1,{id:"o0006----quote-ended-unexpectedly",children:["O0006 - ",(0,t.jsx)(n.code,{children:"``"})," Quote ended unexpectedly"]})}),"\n",(0,t.jsx)(n.h2,{id:"error",children:"Error"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-erlang",children:"    %% @doc To execute the program, call `main`\n    %% Here is some code:\n    %% `` 'erlang@localhost' ``\n%%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: ``-quote ended unexpectedly\n    main() ->\n      ok.\n"})}),"\n",(0,t.jsx)(n.h2,{id:"explanation",children:"Explanation"}),"\n",(0,t.jsxs)(n.p,{children:["Double back-quotes in EDoc are used to quote text containing single ",(0,t.jsx)(n.code,{children:"'"})," characters.\nIf you are getting this error, you are most likely using ",(0,t.jsx)(n.em,{children:"markdown-style"})," monospace quoting instead of ",(0,t.jsx)(n.em,{children:"EDoc's"})," monospace quotes. To fix the error, replace the last occurrence of ",(0,t.jsx)(n.code,{children:"``"})," with ",(0,t.jsx)(n.code,{children:"''"}),":"]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-erlang",children:"    %% @doc To execute the program, call `main`\n    %% Here is some code:\n    %% `` 'erlang@localhost' ''\n    main() ->\n      ok.\n"})}),"\n",(0,t.jsxs)(n.p,{children:["You can read more about verbatim quoting in EDoc ",(0,t.jsx)(n.a,{href:"https://www.erlang.org/doc/apps/edoc/chapter.html#verbatim-quoting",children:"here"}),"."]})]})}function u(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(d,{...e})}):d(e)}},28453:(e,n,r)=>{r.d(n,{R:()=>c,x:()=>s});var o=r(96540);const t={},a=o.createContext(t);function c(e){const n=o.useContext(a);return o.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function s(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(t):e.components||t:c(e.components),o.createElement(a.Provider,{value:n},e.children)}}}]);