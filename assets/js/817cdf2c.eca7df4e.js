"use strict";(self.webpackChunkstaticdocs_starter=self.webpackChunkstaticdocs_starter||[]).push([[6491],{57497:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>o,contentTitle:()=>t,default:()=>u,frontMatter:()=>l,metadata:()=>a,toc:()=>d});const a=JSON.parse('{"id":"erlang-error-index/l/L1318","title":"L1318 - Expression Updates a Literal","description":"Error","source":"@site/docs/erlang-error-index/l/L1318.md","sourceDirName":"erlang-error-index/l","slug":"/erlang-error-index/l/L1318","permalink":"/erlang-language-platform/docs/erlang-error-index/l/L1318","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":318,"frontMatter":{"sidebar_position":318},"sidebar":"tutorialSidebar","previous":{"title":"L1317 - Format Error","permalink":"/erlang-language-platform/docs/erlang-error-index/l/L1317"},"next":{"title":"L1500 - Unused Include","permalink":"/erlang-language-platform/docs/erlang-error-index/l/L1500"}}');var s=r(74848),i=r(28453);const l={sidebar_position:318},t="L1318 - Expression Updates a Literal",o={},d=[{value:"Error",id:"error",level:2},{value:"Explanation",id:"explanation",level:2}];function c(e){const n={code:"code",h1:"h1",h2:"h2",header:"header",p:"p",pre:"pre",...(0,i.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.header,{children:(0,s.jsx)(n.h1,{id:"l1318---expression-updates-a-literal",children:"L1318 - Expression Updates a Literal"})}),"\n",(0,s.jsx)(n.h2,{id:"error",children:"Error"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-erlang",children:"  -define(DEFAULT, #{a => 1}).\n\n  updated(Value) ->\n    ?DEFAULT#{a => Value}.\n%%  ^^^^^^^^ warning: expression updates a literal\n"})}),"\n",(0,s.jsx)(n.h2,{id:"explanation",children:"Explanation"}),"\n",(0,s.jsx)(n.p,{children:"The warning occurs when a map or a record is updated using the following syntaxes:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-erlang",children:"> #{a => b}#{c => d}\n#{c => d,a => b}\n"})}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-erlang",children:"> rd(my_record, {a, b}). %% rd/2 allows you to define an Erlang record from a shell\n> #my_record{a = 1}#my_record{a = 2}.\n#my_record{a = 2,b = undefined}\n"})}),"\n",(0,s.jsx)(n.p,{children:"While this is valid Erlang syntax, this behaviour is usually not intentional and the result of a missing comma in a list of elements. Consider, for example:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-erlang",children:"my_list() ->\n  [ #{a => 1} %% Missing comma here!\n    #{a => 2}\n  ].\n"})}),"\n",(0,s.jsxs)(n.p,{children:["Which results in ",(0,s.jsx)(n.code,{children:"[#{a => 2}]"}),"."]}),"\n",(0,s.jsxs)(n.p,{children:["To fix the issue, just add the missing comma. If the update is intentional, a common (but ugly) workaround to silent the linter is to wrap the first map/record in a ",(0,s.jsx)(n.code,{children:"begin/end"})," block,\nwhich will avoid any additional runtime cost. As an example, you could rewrite the following:"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-erlang",children:"  -define(DEFAULT, #{a => 1}).\n\n  updated(Value) ->\n    ?DEFAULT#{a => Value}.\n"})}),"\n",(0,s.jsx)(n.p,{children:"Into:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-erlang",children:"  -define(DEFAULT, #{a => 1}).\n\n  updated(Value) ->\n    begin ?DEFAULT end#{a => Value}.\n"})})]})}function u(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(c,{...e})}):c(e)}},28453:(e,n,r)=>{r.d(n,{R:()=>l,x:()=>t});var a=r(96540);const s={},i=a.createContext(s);function l(e){const n=a.useContext(i);return a.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function t(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:l(e.components),a.createElement(i.Provider,{value:n},e.children)}}}]);