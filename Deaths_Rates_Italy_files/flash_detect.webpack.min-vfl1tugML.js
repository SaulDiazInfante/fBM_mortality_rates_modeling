define(function(){return(function(r){function n(t){if(e[t])return e[t].exports;var i=e[t]={i:t,l:!1,exports:{}};return r[t].call(i.exports,i,i.exports,n),i.l=!0,i.exports}var e={};return n.m=r,n.c=e,n.d=function(r,e,t){n.o(r,e)||Object.defineProperty(r,e,{enumerable:!0,get:t})},n.r=function(r){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(r,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(r,"__esModule",{value:!0})},n.t=function(r,e){if(1&e&&(r=n(r)),8&e)return r;if(4&e&&"object"==typeof r&&r&&r.__esModule)return r;var t=Object.create(null);if(n.r(t),Object.defineProperty(t,"default",{enumerable:!0,value:r}),2&e&&"string"!=typeof r)for(var i in r)n.d(t,i,function(n){return r[n]}.bind(null,i));return t},n.n=function(r){var e=r&&r.__esModule?function(){return r.default}:function(){return r};return n.d(e,"a",e),e},n.o=function(r,n){return Object.prototype.hasOwnProperty.call(r,n)},n.p="",n(n.s=0)})({"./flash_detect.js":function(r,n){var e=new(function(){var r=this;r.installed=!1,r.raw="",r.major=-1,r.minor=-1,r.revision=-1,r.revisionStr="";var n=[{name:"ShockwaveFlash.ShockwaveFlash.7",version:function(r){return e(r)}},{name:"ShockwaveFlash.ShockwaveFlash.6",version:function(r){var n="6,0,21";try{r.AllowScriptAccess="always",n=e(r)}catch(r){}return n}},{name:"ShockwaveFlash.ShockwaveFlash",version:function(r){return e(r)}}],e=function(r){var n=-1;try{n=r.GetVariable("$version")}catch(r){}return n},t=function(r){var n=-1;try{n=new ActiveXObject(r)}catch(r){n={activeXError:!0}}return n},o=function(r){var n=r.split(",");return{raw:r,major:parseInt(n[0].split(" ")[1],10),minor:parseInt(n[1],10),revision:parseInt(n[2],10),revisionStr:n[2]}},a=function(r){var n=r.split(/ +/),e=n[2].split(/\./),t=n[3];return{raw:r,major:parseInt(e[0],10),minor:parseInt(e[1],10),revisionStr:t,revision:s(t)}},s=function(n){return parseInt(n.replace(/[a-zA-Z]/g,""),10)||r.revision};r.majorAtLeast=function(n){return r.major>=n},r.minorAtLeast=function(n){return r.minor>=n},r.revisionAtLeast=function(n){return r.revision>=n},r.versionAtLeast=function(n){var e=[r.major,r.minor,r.revision],t=Math.min(e.length,arguments.length);for(i=0;i<t;i++){if(e[i]>=arguments[i]){if(i+1<t&&e[i]==arguments[i])continue;return!0}return!1}},r.FlashDetect=(function(){if(navigator.plugins&&navigator.plugins.length>0){var e="application/x-shockwave-flash",i=navigator.mimeTypes;if(i&&i[e]&&i[e].enabledPlugin&&i[e].enabledPlugin.description){var s=i[e].enabledPlugin.description,u=a(s);r.raw=u.raw,r.major=u.major,r.minor=u.minor,r.revisionStr=u.revisionStr,r.revision=u.revision,r.installed=!0}}else if(navigator.appVersion.indexOf("Mac")==-1&&window.execScript)for(var s=-1,c=0;c<n.length&&s==-1;c++){var l=t(n[c].name);if(!l.activeXError&&(r.installed=!0,s=n[c].version(l),s!=-1)){var u=o(s);r.raw=u.raw,r.major=u.major,r.minor=u.minor,r.revision=u.revision,r.revisionStr=u.revisionStr}}})()});e.JS_RELEASE="1.0.4",r.exports=e},0:function(r,n,e){r.exports=e("./flash_detect.js")}})});
//# sourceMappingURL=flash_detect.webpack.min.js-vfl8FoAb-.map