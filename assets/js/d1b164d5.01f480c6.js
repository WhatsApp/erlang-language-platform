"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[2070],{40538:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>l,contentTitle:()=>s,default:()=>u,frontMatter:()=>i,metadata:()=>o,toc:()=>d});const o=JSON.parse('{"id":"erlang-error-index/w/W0020","title":"W0020 - Unused Include","description":"Error","source":"@site/docs/erlang-error-index/w/W0020.md","sourceDirName":"erlang-error-index/w","slug":"/erlang-error-index/w/W0020","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0020","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":20,"frontMatter":{"sidebar_position":20},"sidebar":"tutorialSidebar","previous":{"title":"W0017 - Undefined Function","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0017"},"next":{"title":"W0021 - Cannot Evaluate Common Test Callbacks","permalink":"/erlang-language-platform/docs/erlang-error-index/w/W0021"}}');var t=r(74848),a=r(28453);const i={sidebar_position:20},s="W0020 - Unused Include",l={},d=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}];function c(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",header:"header",p:"p",pre:"pre",...(0,a.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.header,{children:(0,t.jsx)(n.h1,{id:"w0020---unused-include",children:"W0020 - Unused Include"})}),"\n",(0,t.jsx)(n.h2,{id:"error",children:"Error"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-erlang",children:'//- /include/foo.hrl\n  -define(FOO,3).\n\n//- /src/foo.erl\n  -module(foo).\n  -include("foo.hrl").\n%%^^^^^^^^^^^^^^^^^^^^ \ud83d\udca1 warning: Unused file: foo.hrl\n'})}),"\n",(0,t.jsx)(n.h2,{id:"explanation",children:"Explanation"}),"\n",(0,t.jsxs)(n.p,{children:["The warning message indicates that no definitions or attributes contained in the ",(0,t.jsx)(n.code,{children:"foo.hrl"})," header are used in the ",(0,t.jsx)(n.code,{children:"foo"})," module and therefore the ",(0,t.jsx)(n.code,{children:"include"})," statement can be safely removed from ",(0,t.jsx)(n.code,{children:"foo.erl"}),"."]}),"\n",(0,t.jsxs)(n.p,{children:["In case of a false positive, please use the ",(0,t.jsxs)(n.a,{href:"/erlang-language-platform/docs/erlang-error-index/#ignoring-diagnostics",children:["standard ",(0,t.jsx)(n.code,{children:"elp:ignore"})," mechanism"]})," to temporarily silence the warning and report this as a bug."]})]})}function u(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(c,{...e})}):c(e)}},28453:(e,n,r)=>{r.d(n,{R:()=>i,x:()=>s});var o=r(96540);const t={},a=o.createContext(t);function i(e){const n=o.useContext(a);return o.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function s(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(t):e.components||t:i(e.components),o.createElement(a.Provider,{value:n},e.children)}}}]);