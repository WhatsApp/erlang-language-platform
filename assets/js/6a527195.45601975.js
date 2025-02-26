"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[1083],{34137:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>c,contentTitle:()=>s,default:()=>p,frontMatter:()=>i,metadata:()=>t,toc:()=>d});const t=JSON.parse('{"id":"erlang-error-index/w/W0038","title":"W0038 - Old EDoc Syntax","description":"Warning","source":"@site/docs/erlang-error-index/w/W0038.md","sourceDirName":"erlang-error-index/w","slug":"/erlang-error-index/w/W0038","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0038","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":38,"frontMatter":{"sidebar_position":38},"sidebar":"tutorialSidebar","previous":{"title":"W0037 - Unspecific include","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0037"}}');var a=r(74848),o=r(28453);const i={sidebar_position:38},s="W0038 - Old EDoc Syntax",c={},d=[{value:"Warning",id:"warning",level:2},{value:"Explanation",id:"explanation",level:2}];function l(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",header:"header",p:"p",pre:"pre",...(0,o.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.header,{children:(0,a.jsx)(n.h1,{id:"w0038---old-edoc-syntax",children:"W0038 - Old EDoc Syntax"})}),"\n",(0,a.jsx)(n.h2,{id:"warning",children:"Warning"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-erlang",children:"    -module(main).\n    %% @doc This is the main function documentation.\n    %%<^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: Old EDoc syntax\n    main() ->\n      dep().\n\n    dep() -> ok.\n"})}),"\n",(0,a.jsx)(n.h2,{id:"explanation",children:"Explanation"}),"\n",(0,a.jsxs)(n.p,{children:["The code is using ",(0,a.jsx)(n.a,{href:"https://www.erlang.org/doc/apps/edoc/chapter.html",children:"EDoc style comments"})," to describe a module or a function.\nStarting from OTP 27, EDoc style comments are deprecated in favour of the ",(0,a.jsx)(n.a,{href:"https://www.erlang.org/eeps/eep-0059",children:"EEP59"})," syntax.\nPlease replace the old style comment with the new syntax."]}),"\n",(0,a.jsx)(n.p,{children:"Example:"}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-erlang",children:'    -module(main).\n    -doc "This is the main function documentation.".\n    main() ->\n      dep().\n\n    dep() -> ok.\n'})})]})}function p(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(l,{...e})}):l(e)}},28453:(e,n,r)=>{r.d(n,{R:()=>i,x:()=>s});var t=r(96540);const a={},o=t.createContext(a);function i(e){const n=t.useContext(o);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function s(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:i(e.components),t.createElement(o.Provider,{value:n},e.children)}}}]);