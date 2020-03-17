define("modules/clean/appshell/account",["require","exports","modules/core/uri"],function(e,t,r){"use strict";function a(e,t){var r=window.location.href,a=n(r,e.viewer,e.viewer.getActiveUser().userId,t);a&&(sessionStorage.setItem("Ensemble.switchedAccounts","true"),window.location.href=a)}function n(e,t,r,a){if(a===r)return null;var n=s(e);if(parseInt(a.toString(),10)!==a)throw new Error("switchAccount takes an integer subjectUidToSwitchTo. got: "+a);var i=t.getUserById(a).role,c=n.getPath();return"/history"===c.substr(0,8)||"/event_details"===c.substr(0,14)?(n.setPath("/"+i),n.toString()):("/team"===c.substr(0,5)?n.setPath("/personal"):"/photos"===c.substr(0,7)?n.setPath("/work"):"/work"===c?n.setPath("/personal"):"/personal"===c&&n.setPath("/work"),o(c)?(n.setPath("/"+i),n.toString()):(n.setQuery({role:i}),n.toString()))}function o(e){return"/scl"===e.substr(0,4)||"/sh"===e.substr(0,3)||"/s"===e.substr(0,2)}function s(e){for(var t=r.URI.parse(e),a=t.getPath(),n=["/work","/personal","/home","/history","/s","/sh","/scl","/team/admin/members","/team/admin/team_folders","/team/admin/content","/team/admin/billing","/team/admin/settings","/enterprise_console/settings","/enterprise_console/teams"],o=0,s=n;o<s.length;o++){var i=s[o];if(a===i||0===a.indexOf(i+"/")){a=i;break}}return 0===a.indexOf("/share/")&&(a="/share"),"/search/personal"===a?a="/personal":"/search/work"===a?a="/work":"/team_checklist"===a?a="/h":"/business/billing"===a&&(a="/team/admin/billing"),t.setPath(a),t.setQuery().setFragment(),t}Object.defineProperty(t,"__esModule",{value:!0}),t.switchAccount=a,t._computeAccountSwitchUrl=n,t.isSharedLinkCase=o,t.computeFeatureBaseUrlFromUrl=s}),define("modules/clean/react/maestro_nav/maestro_nav",["require","exports","tslib","external/classnames","react","spectrum/vertically_fixed","modules/clean/appshell/account","modules/clean/react/css","modules/clean/react/maestro_nav/shared_code/api","modules/clean/react/maestro_nav/shared_code/maestro_sidebar","modules/clean/react/maestro_nav/shared_code/strings","modules/clean/react/maestro_nav/util/post_tti_components","modules/clean/upsell/prompt_event_emitter","modules/clean/viewer","modules/core/browser","modules/core/xhr"],function(e,t,r,a,n,o,s,i,c,l,u,d,m,p,f,v){"use strict";function _(e,t){function r(e){return s.computeFeatureBaseUrlFromUrl(e).getPath()}function a(e,t,a){var n=a.map(r);return n.indexOf(e)!==-1&&n.indexOf(t)!==-1}var n=r(t),o=r(e);return!!(a(n,o,["/home","/work","/personal","/history","/s","/sh","/scl"])||a(n,o,["/team/admin","/team/admin/members"])||a(n,o,["/organization","/enterprise_console/teams"]))||n===o}Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importDefault(a),n=r.__importDefault(n),f=r.__importStar(f);var h=function(){return p.Viewer.get_viewer().get_users().map(function(e){return e.id})},g=function(e,t){v.sendXhr("/teamswalogger",{event_name:e,extra:JSON.stringify(t),for_uids:JSON.stringify(h())})};t.isFeatureActive=_;var w=function(){return window.location.href},E=function(e){return n.default.createElement("button",{className:"maestro_nav__toggle",onClick:e.onClick,"aria-label":e.navToggleAriaLabel},n.default.createElement("svg",{width:"24",height:"18",viewBox:"0 0 24 18"},n.default.createElement("g",{fill:"#637282",fillRule:"evenodd"},n.default.createElement("rect",{x:"0",y:"0",width:"24",height:"2",rx:"1"}),n.default.createElement("rect",{x:"0",y:"8",width:"24",height:"2",rx:"1"}),n.default.createElement("rect",{x:"0",y:"16",width:"24",height:"2",rx:"1"}))))},b=function(e){return n.default.createElement("button",{className:"maestro_nav__curtain",onClick:e.onClick,"aria-label":e.navToggleAriaLabel})},N=(function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return r.__extends(t,e),t.prototype.render=function(){return n.default.createElement(P,{activeAccountId:this.props.activeAccountId,viewerInfo:this.props.viewerInfo,products:this.props.products,isFeatureActive:_,experiments:this.props.experiments})},t})(n.default.Component),P=(function(t){function i(a){var o=t.call(this,a)||this;o.navigateToUrl=function(e){f.redirect(e)},o.navSwitchAccount=function(e){var t=p.Viewer.get_viewer();s.switchAccount(window.ensemble,t.get_user_by_role(e,!0).id)},o.handleNavToggle=function(){o.setState({isExpanded:!o.state.isExpanded})},o.handleNavigationClick=function(t,a){if(a.isBlocked){var n=void 0;switch(a.blockerModule){case"modules/clean/teams/locked_team_nav_blocker":n=new Promise(function(t,r){e(["modules/clean/teams/locked_team_nav_blocker"],t,r)}).then(r.__importStar);break;default:return}n.then(function(e){return e.blocker.blockNav(a)})}},o.renderNewAdminCoachMark=function(e){var t=o.props.activeAccountId,r=o.state.experiments;return Boolean(f.get_href().match(/\h/)&&"admin"===e.uniqueName&&"V1"===o.expSubgrowthBizTeamsMoreAdmins)?n.default.createElement(d.PostTTINewAdminCoachMark,{activeAccountId:t,tooltipContent:r.newAdminCoachMarkStrings}):null},o.renderLeftHA=function(){return o.state.haProps?n.default.createElement(d.PostTTILeftHA,r.__assign({},o.state.haProps)):null};var i=o.getActiveProduct(o.props.products),c=o.getActiveFeature(i);return o.state={isExpanded:!1,products:o.props.products,teamSize:null,numMembersString:null,viewerInfo:o.props.viewerInfo,activeProduct:i,activeFeature:c,experiments:{},fetching:!0},o}return r.__extends(i,t),Object.defineProperty(i.prototype,"expSubgrowthBizTeamsMoreAdmins",{get:function(){var e=this.state.experiments;return(void 0===e?{}:e).subgrowth_biz_teams_more_admins},enumerable:!0,configurable:!0}),i.prototype.getActiveProduct=function(e){var t=this;return e.find(function(e){return t._productIsActive(e)})||null},i.prototype._productIsActive=function(e){return!!this.props.isFeatureActive(e.url,w())||!!this.getActiveFeature(e)},i.prototype.getActiveFeature=function(e){var t=this;return e?e.features.find(function(e){return t.props.isFeatureActive(e.url,w())})||null:null},i.prototype.getActiveProductAndFeature=function(e){var t=this.getActiveProduct(e);return{activeProduct:t,activeFeature:this.getActiveFeature(t)}},i.prototype.componentDidMount=function(){var e=this;c.fetchNavData(this.props.activeAccountId,function(t,r,a,n,o){var s=window.performance&&window.performance.mark&&window.performance.measure&&window.location.search.indexOf("show_debug_spans")>-1;s&&window.performance.mark("MaestroNav data setState start"),n=n||e.state.viewerInfo;var i=e.getActiveProductAndFeature(t),c=i.activeProduct,l=i.activeFeature;e.setState({products:t,teamSize:r,numMembersString:a,viewerInfo:n,activeProduct:c,activeFeature:l,experiments:o,fetching:!1},function(){s&&(window.performance.mark("MaestroNav data setState end"),window.performance.measure("MaestroNav data setState","MaestroNav data setState start","MaestroNav data setState end"))})}),m.promptBufferedEventEmitter.on(m.Events.ON_HA_LOADED,function(t){e.setState({haProps:t})})},i.prototype.setTitle=function(e,t){t&&t.pageTitle?document.title=t.pageTitle:e&&e.pageTitle&&(document.title=e.pageTitle)},i.prototype.productHasFeatures=function(e){return!!e&&e.features.length>0},i.prototype.render=function(){var e,t=this.state.experiments||{},r=t.inPlusToProTrials,s=t.featuresConfig;e=this.productHasFeatures(this.state.activeProduct)?"Feature":"Product",this.setTitle(this.state.activeProduct,this.state.activeFeature);var i=a.default({"maestro-nav__offset-container":!0,"maestro-nav__expanded":this.state.isExpanded});return n.default.createElement(o.VerticallyFixed,{className:i},n.default.createElement(E,{onClick:this.handleNavToggle,navToggleAriaLabel:u.navToggleAriaLabel()}),n.default.createElement(b,{onClick:this.handleNavToggle,navToggleAriaLabel:u.navToggleAriaLabel()}),n.default.createElement(l.MaestroSidebar,{activeFeature:this.state.activeFeature,activeProduct:this.state.activeProduct,onNavigationClick:this.handleNavigationClick,products:this.state.products,navType:e,navigateToUrl:this.navigateToUrl,switchAccount:this.navSwitchAccount,viewerInfo:this.state.viewerInfo,teamSize:this.state.teamSize,numMembersString:this.state.numMembersString,logger:g,renderNewAdminCoachMark:this.renderNewAdminCoachMark,renderLeftHA:this.renderLeftHA,inPlusToProTrials:r,fetching:this.state.fetching,featuresConfig:s}))},i})(n.default.Component);t.MaestroNavInternal=P;var T=i.requireCssWithComponent(N,["/static/css/maestro-nav/maestro-nav-vflKYgQCj.css"]);t.MaestroNav=T;var A=function(e){return n.default.createElement("div",{className:"maestro-nav-component"},n.default.createElement(T,r.__assign({},e)))};t.MaestroNavWithWrapper=A,t.RootComponent=function(e){return n.default.createElement(A,r.__assign({},e))}}),define("modules/clean/react/maestro_nav/post_tti/api",["require","exports","tslib","modules/clean/web_timing_logger","modules/clean/react/async/loadable"],function(e,t,r,a,n){"use strict";function o(){return r.__awaiter(this,void 0,Promise,function(){return r.__generator(this,function(t){switch(t.label){case 0:return[4,a.waitForTTI()];case 1:return t.sent(),[4,new Promise(function(t,r){e(["modules/clean/react/maestro_nav/post_tti/interface"],t,r)}).then(r.__importStar)];case 2:return[2,t.sent()]}})})}function s(e){var t=this,a=e.getPostTTIRenderer,s=e.preTTIRenderer;return n.Loadable({loader:function(){return r.__awaiter(t,void 0,void 0,function(){var e;return r.__generator(this,function(t){switch(t.label){case 0:return e=a,[4,o()];case 1:return[2,e.apply(void 0,[t.sent()])]}})})},loading:s})}Object.defineProperty(t,"__esModule",{value:!0}),t.waitForHomeTTI=o,t.PostNavTTIComponent=s}),define("modules/clean/react/maestro_nav/shared_code/api",["require","exports","modules/clean/react/maestro_nav/shared_code/constants"],function(e,t,r){"use strict";function a(e,t){var a=new XMLHttpRequest,n="https://"+r.WWW_HOST+"/nav_menu?_subject_account_id="+e+"&nav_data_version=4";a.open("GET",n,!0),a.withCredentials=!0,a.onload=function(){if(!(a.status>=200&&a.status<400))throw new Error("Error status "+a.status+" returned from /nav_menu");try{var e=JSON.parse(a.responseText);t(e.products,e.teamSize,e.numMembersString,e.viewerInfo,e.experiments)}catch(e){throw new Error('Error "'+String(e)+'" JSON parsing "'+a.responseText+'" (status: '+a.status+")")}},a.onerror=function(e){var t=e,r=t.message,a=t.colno,n=t.filename,o=t.lineno,s={message:r,colno:a,filename:n,lineno:o};throw new Error('Error calling /nav_menu. "'+JSON.stringify(s)+'"')},a.send()}Object.defineProperty(t,"__esModule",{value:!0}),t.fetchNavData=a}),define("modules/clean/react/maestro_nav/shared_code/constants",["require","exports"],function(e,t){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),t.WWW_HOST="www.dropbox.com"}),define("modules/clean/react/maestro_nav/shared_code/feature-nav",["require","exports","tslib","react","external/classnames","modules/clean/react/maestro_nav/shared_code/lock_icon","modules/clean/user_education/react/user_education_effect"],function(e,t,r,a,n,o,s){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importDefault(a),n=r.__importDefault(n);var i=(function(e){function t(){var t=null!==e&&e.apply(this,arguments)||this;return t.onClick=function(e){return function(r){return t.props.onNavigationClick(r,e)}},t}return r.__extends(t,e),t.prototype.renderDisplaySuperscript=function(e){return a.default.createElement("sup",{className:"maestro-nav__feature-superscript"},e)},t.prototype.render=function(){var e=this,t=this.props,r=t.features,i=t.productDisplayName,c=t.productUrl;return a.default.createElement("ul",{className:"maestro-nav__features"},a.default.createElement("li",null,a.default.createElement("h2",{className:"maestro-nav__features-header"},a.default.createElement("a",{href:c,className:"maestro-nav__features-header-link"},i))),r.map(function(t,r){var i=t.url,c=n.default({"maestro-nav__feature":!0,"maestro-nav__active-feature":t===e.props.activeFeature,"maestro-nav__blocked-feature":t.isBlocked});return a.default.createElement("li",{key:i},a.default.createElement("div",{className:"maestro-nav__feature-wrap"},a.default.createElement(s.UserEducationEffect,{containerName:"FeatureNav",name:t.uniqueName,useSpan:!0},a.default.createElement("a",{href:t.isBlocked?"#":i,className:c,onClick:e.onClick(t)},t.displayName,t.displaySuperscript?e.renderDisplaySuperscript(t.displaySuperscript):void 0,t.isBlocked&&a.default.createElement(o.LockIcon,{className:"maestro-nav__lock"})))))}))},t})(a.default.Component);t.FeatureNav=i}),define("modules/clean/react/maestro_nav/shared_code/footer_nav",["require","exports","tslib","react","external/classnames","react-aria-menubutton","modules/clean/react/maestro_nav/shared_code/constants","modules/clean/react/maestro_nav/shared_code/strings","modules/clean/user_education/react/user_education_effect","modules/clean/user_education/user_education_client"],function(e,t,r,a,n,o,s,i,c,l){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importDefault(a),n=r.__importDefault(n),o=r.__importStar(o);var u,d=function(e){var t=e.className;return a.default.createElement("svg",{className:t,viewBox:"0 0 24 24"},a.default.createElement("g",{id:"Page-1",stroke:"none",strokeWidth:"1",fill:"none",fillRule:"evenodd"},a.default.createElement("g",{id:"form",transform:"translate(-160.000000, -32.000000)",fill:"#000000"},a.default.createElement("g",{id:"form-switcher",transform:"translate(160.000000, 32.000000)"},a.default.createElement("path",{d:"M10.5,9 L8.26000214,9 L12.0100021,4 L15.7600021,9 L13.5,9 L12,7 L10.5,9 Z",id:"Combined-Shape"}),a.default.createElement("path",{d:"M10.5,20 L8.26000214,20 L12.0100021,15 L15.7600021,20 L13.5,20 L12,18 L10.5,20 Z",id:"Combined-Shape",transform:"translate(12.010002, 17.500000) scale(1, -1) translate(-12.010002, -17.500000) "})))))},m=function(e){var t=e.className;return a.default.createElement("svg",{className:t,viewBox:"0 0 24 24"},a.default.createElement("g",{id:"Page-1",stroke:"none",strokeWidth:"1",fill:"none",fillRule:"evenodd"},a.default.createElement("g",{id:"form",transform:"translate(-32.000000, -96.000000)",fill:"#000000"},a.default.createElement("g",{id:"check",transform:"translate(32.000000, 96.000000)"},a.default.createElement("path",{d:"M13.1239594,15.613961 L9.12395935,15.613961 L9.12395935,17.613961 L14.1239594,17.613961 L15.1239594,17.613961 L15.1239594,5.61396103 L13.1239594,5.61396103 L13.1239594,15.613961 Z",id:"Combined-Shape",transform:"translate(12.123959, 11.613961) rotate(-315.000000) translate(-12.123959, -11.613961) "})))))},p={adminConsole:"team/admin/members",teamPage:"team"};(function(e){e[e.ADD_PERSONAL=0]="ADD_PERSONAL",e[e.ADD_WORK=1]="ADD_WORK",e[e.GOTO_WORK=2]="GOTO_WORK",e[e.GOTO_PERSONAL=3]="GOTO_PERSONAL"})(u||(u={}));var f=(function(e){function t(){var t=null!==e&&e.apply(this,arguments)||this;return t.strings=i.footerStrings(),t.handleSelection=function(e){var r=t.props,a=r.activeRole,n=r.navigateToUrl,o=r.switchAccount;switch(e){case u.ADD_PERSONAL:n("https://"+s.WWW_HOST+"/team/join/new_pair");break;case u.ADD_WORK:n("https://"+s.WWW_HOST+"/team?_tk=account_switcher");break;case u.GOTO_PERSONAL:"work"===a&&o("personal");break;case u.GOTO_WORK:"personal"===a&&o("work")}},t.handleMenuButtonClick=function(e){var r=t.props,a=r.logger,n=r.userPlanType;a&&n&&a("clicked_acct_switcher",{user_plan_type:n}),l.UEClient.sendEvent("NavSwitcher","MenuButtonClicked")},t.handleTeamPagePress=function(e){13===e.keyCode&&(e.preventDefault(),l.UEClient.sendEvent("NavSwitcher","TeamPageAction"),t.navigateToTeamPage()),e.stopPropagation()},t.handleTeamPageClick=function(e){e.stopPropagation(),l.UEClient.sendEvent("NavSwitcher","TeamPageAction"),t.navigateToTeamPage()},t.navigateToTeamPage=function(){var e,r=t.props,a=r.isAdmin,n=r.isPaidTeam;e=a&&n?p.adminConsole:p.teamPage,t.props.navigateToUrl("https://"+s.WWW_HOST+"/"+e)},t.MenuItem=function(e){return a.default.createElement(o.MenuItem,{className:n.default({"maestro-nav-switcher-menu-item":!0,"maestro-nav-switcher-menu-item--selected":e.selected}),role:"menuitemradio","aria-checked":e.selected?"true":"false",value:e.value},a.default.createElement(m,{className:"maestro-nav-switcher-menu-item__checkmark"}),a.default.createElement("div",{className:"maestro-nav-switcher-menu-item__info"},a.default.createElement("div",{className:"maestro-nav-switcher-menu-item__title"},e.label),a.default.createElement(c.UserEducationEffect,{containerName:"NavSwitcher",name:"ActionMenuItem"+(e.selected?"Selected":"")},a.default.createElement("div",{className:"maestro-nav-switcher-menu-item__action"},e.action))))},t.Menu=function(){var e,r,n,s=t.strings,i=t.props,l=i.teamName,d=i.isPaired,m=i.activeRole,p="personal"===m;d||p?(e=s.personalString,r=null,n=u.GOTO_PERSONAL):(e=s.addPersonalDropboxString,r=a.default.createElement("span",{className:"maestro-nav-switcher-menu-item__action-text"},s["2GbFreeString"]),n=u.ADD_PERSONAL);var f,v,_;if(!d&&p)f=s.tryDropboxBusinessFreeString,_=u.ADD_WORK;else{f=l||s.workString;var h=t.getTeamActionLabel();v=p?a.default.createElement("span",{className:"maestro-nav-switcher-menu-item__action-text"},h):a.default.createElement("button",{onKeyDown:t.handleTeamPagePress,onClick:t.handleTeamPageClick},h),_=u.GOTO_WORK}return a.default.createElement(o.Menu,{className:"maestro-nav-switcher-menu"},a.default.createElement(c.UserEducationEffect,{containerName:"NavSwitcher",name:"AriaMenuButton.Menu"},a.default.createElement(t.MenuItem,{selected:!p,label:f,action:v,value:_}),a.default.createElement(t.MenuItem,{selected:p,label:e,action:r,value:n})))},t.navSwitcherInfo=function(){var e,r,n=t.strings,o=t.props,s=o.teamName,i=o.numMembersString,c=o.activeRole;return"personal"===c?(e=n.personalString,r=n.onlyYouString):(e=s,r=null==i?n.workDropboxString:i),a.default.createElement("div",{className:"maestro-nav-switcher-button__info"},a.default.createElement("div",{className:"maestro-nav-switcher-button__title"},e),a.default.createElement("div",{className:"maestro-nav-switcher-button__label"},r))},t.MenuButton=function(){return a.default.createElement(o.Button,{className:"maestro-nav-switcher-button"},a.default.createElement("div",{className:"maestro-nav-switcher-button__content",onClick:t.handleMenuButtonClick},a.default.createElement(t.navSwitcherInfo,null),a.default.createElement(d,{className:"maestro-nav-switcher-button__icon"})))},t}return r.__extends(t,e),t.prototype.getTeamActionLabel=function(){var e=this.strings,t=this.props,r=t.teamSize,a=t.numMembersString,n=t.activeRole,o="personal"===n;return null==r?o?e.workDropboxString:e.groupsAndMembersString:o||r<50?a:e.groupsAndMembersString},t.prototype.render=function(){return this.props.makeSwitcherReadOnly?a.default.createElement("div",{className:"maestro-nav-switcher"},a.default.createElement("div",{className:"maestro-nav-switcher-button__content"},a.default.createElement(this.navSwitcherInfo,null))):a.default.createElement(o.Wrapper,{className:"maestro-nav-switcher",onSelection:this.handleSelection,closeOnSelection:!1},a.default.createElement(this.MenuButton,null),a.default.createElement(this.Menu,null))},t})(a.default.Component),v=(function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return r.__extends(t,e),Object.defineProperty(t.prototype,"showReadOnly",{get:function(){var e=this.props,t=e.inPlusToProTrials,r=e.fetching,a=e.featuresConfig,n=e.viewerInfo;if(r||n.isGuestAdmin)return!0;var o=n.activeRole,s=n.isPaired,i="personal"===o,c=a&&a.hideDropboxBusinessUpsell;return(Boolean(c)||Boolean(t))&&!s&&i},enumerable:!0,configurable:!0}),t.prototype.render=function(){var e=this.props,t=e.teamSize,r=e.numMembersString,n=e.viewerInfo,o=e.navigateToUrl,s=e.switchAccount,i=e.logger,l=n.activeRole,u=n.isAdmin,d=n.isPaidTeam,m=n.isPaired,p=n.teamName,v=n.userPlanType;return a.default.createElement("div",{className:"maestro-nav-footer"},a.default.createElement(c.UserEducationEffect,{containerName:"FooterNav",name:"NavSwitcher"},a.default.createElement(f,{teamSize:t,numMembersString:r,teamName:p,isPaired:m,navigateToUrl:o,switchAccount:s,activeRole:l,isAdmin:u,isPaidTeam:d,userPlanType:v,logger:i,makeSwitcherReadOnly:this.showReadOnly})))},t})(a.default.Component);t.FooterNav=v}),define("modules/clean/react/maestro_nav/shared_code/lock_icon",["require","exports","tslib","react"],function(e,t,r,a){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importDefault(a),t.LockIcon=function(e){return a.default.createElement("svg",r.__assign({},e,{xmlns:"http://www.w3.org/2000/svg",role:"img",width:"8px",height:"10px",viewBox:"0 0 8 10"}),a.default.createElement("title",null,e.alt||"Lock"),a.default.createElement("g",{"fill-rule":"evenodd",style:{fill:"none"}},a.default.createElement("path",{d:"M1.5,5.0233365 C1.61081383,5.07571391 1.79156144,5.1271516 2.02641537,5.1695923 C2.53116508,5.26080616 3.23772228,5.30304304 3.99147442,5.29018883 C4.72697272,5.27764591 5.44536431,5.2134662 5.96818405,5.1130429 C6.19749663,5.0689965 6.37973948,5.0195837 6.5,4.97094138 C6.5,4.61127567 6.5,4.5367094 6.5,3.97982189 L6.5,2.55833246 C6.5,1.44077827 5.39680687,0.5 4,0.5 C2.60319313,0.5 1.5,1.44077827 1.5,2.55833246 L1.5,4.08873795 C1.5,4.48924589 1.5,4.52037046 1.5,5.0233365 Z",style:{stroke:"#6a7c8f",strokeWidth:1}}),a.default.createElement("rect",{x:"0.5",y:"4.5",width:"7",height:"5",rx:"1",style:{fill:"#6a7c8f",stroke:"#6a7c8f",strokeWidth:1}})))}}),define("modules/clean/react/maestro_nav/shared_code/maestro_sidebar",["require","exports","tslib","react","modules/clean/react/maestro_nav/shared_code/constants","modules/clean/react/maestro_nav/shared_code/footer_nav","modules/clean/react/maestro_nav/shared_code/nested-nav"],function(e,t,r,a,n,o,s){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importStar(a);var i=(function(e){function t(t){return e.call(this,t)||this}return r.__extends(t,e),t.prototype.renderContents=function(){return a.createElement(s.NestedNav,{activeFeature:this.props.activeFeature,activeProduct:this.props.activeProduct,onNavigationClick:this.props.onNavigationClick,onHomeClick:this.props.onHomeClick,nestedNavLevel:this.props.navType,products:this.props.products,renderNewAdminCoachMark:this.props.renderNewAdminCoachMark,renderLeftHA:this.props.renderLeftHA,dropboxLogoColor:this.props.dropboxLogoColor,dropboxLogoUrl:this.props.viewerInfo.isGuestAdmin?null:"https://"+n.WWW_HOST+"/"})},t.prototype.renderFooter=function(){return a.createElement(o.FooterNav,{navigateToUrl:this.props.navigateToUrl,switchAccount:this.props.switchAccount,viewerInfo:this.props.viewerInfo,teamSize:this.props.teamSize,numMembersString:this.props.numMembersString,logger:this.props.logger,inPlusToProTrials:this.props.inPlusToProTrials,fetching:this.props.fetching,featuresConfig:this.props.featuresConfig})},t.prototype.render=function(){return a.createElement("div",{className:"maestro-nav__container"},this.renderContents(),this.renderFooter())},t})(a.Component);t.MaestroSidebar=i}),define("modules/clean/react/maestro_nav/shared_code/nested-nav",["require","exports","tslib","react","modules/clean/react/maestro_nav/shared_code/dropbox_logo","modules/clean/react/maestro_nav/shared_code/feature-nav","modules/clean/react/maestro_nav/shared_code/product-nav","modules/clean/react/maestro_nav/shared_code/strings"],function(e,t,r,a,n,o,s,i){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importDefault(a),t.LeftArrowSVGIcon=function(e){var t=e.className;return a.default.createElement("svg",{className:t,width:"24px",height:"24px",viewBox:"0 0 24 24",version:"1.1"},a.default.createElement("title",null),a.default.createElement("g",{id:"arrow",transform:"translate(-32.000000, -32.000000)"},a.default.createElement("g",{id:"arrow-left",transform:"translate(32.000000, 32.000000)"},a.default.createElement("polygon",{id:"Combined-Shape",points:"12.9497475 7.05000305 8 11.9997505 12.9497475 16.949498 14.363961 15.5352844 10.8284271 11.9997505 14.363961 8.46421661"}))))};var c=(function(e){function c(){return null!==e&&e.apply(this,arguments)||this}return r.__extends(c,e),c.prototype.renderContents=function(){if("Feature"===this.props.nestedNavLevel&&this.props.activeProduct){var e=this.props.activeProduct;return a.default.createElement(o.FeatureNav,{activeFeature:this.props.activeFeature,features:e.features,onNavigationClick:this.props.onNavigationClick,productDisplayName:e.displayName,productUrl:e.url})}return a.default.createElement(s.ProductNav,{products:this.props.products,activeProduct:this.props.activeProduct,onNavigationClick:this.props.onNavigationClick,renderNewAdminCoachMark:this.props.renderNewAdminCoachMark})},c.prototype.renderDropboxLogo=function(){var e=this.props,r=e.nestedNavLevel,o=e.onHomeClick,s=e.dropboxLogoColor,c="Feature"===r,l=a.default.createElement(t.LeftArrowSVGIcon,{className:"maestro-nav__caret"}),u=a.default.createElement(n.DropboxLogo,{className:"maestro-nav__logo","aria-label":i.homeAriaLabel(),color:s});return this.props.dropboxLogoUrl?a.default.createElement("a",{className:"maestro-nav__home-button",href:this.props.dropboxLogoUrl,onClick:o},u,c&&l):u},c.prototype.render=function(){return a.default.createElement("div",{className:"maestro-nav__panel"},this.renderDropboxLogo(),a.default.createElement("div",{className:"maestro-nav__contents"},this.renderContents()),this.props.renderLeftHA&&this.props.renderLeftHA())},c})(a.default.Component);t.NestedNav=c}),define("modules/clean/react/maestro_nav/shared_code/product-nav",["require","exports","tslib","react","external/classnames","modules/clean/react/maestro_nav/shared_code/lock_icon","modules/clean/user_education/react/user_education_effect"],function(e,t,r,a,n,o,s){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importDefault(a),n=r.__importDefault(n);var i=(function(e){function t(){var t=null!==e&&e.apply(this,arguments)||this;return t.onClick=function(e){return function(r){return t.props.onNavigationClick(r,e)}},t}return r.__extends(t,e),t.prototype.renderDisplaySuperscript=function(e,t){if(e)switch(t){case"badge":return a.default.createElement("span",{className:"maestro-nav__product-badge"},e);default:return a.default.createElement("sup",{className:"maestro-nav__product-superscript"},e)}},t.prototype.render=function(){var e=this,t=this.props,r=t.activeProduct,i=t.products,c=t.renderNewAdminCoachMark;return a.default.createElement("ul",{className:"maestro-nav__products"},i.map(function(t){var i=n.default({"maestro-nav__product":!0,"maestro-nav__active-product":t===r,"maestro-nav__blocked-feature":t.isBlocked}),l=t.openInNewPage?"_blank":"_self",u=e.renderDisplaySuperscript(t.displaySuperscript,t.displaySuperscriptStyle);return a.default.createElement("li",{key:t.url},a.default.createElement("div",{className:"maestro-nav__product-wrapper"},a.default.createElement(s.UserEducationEffect,{containerName:"ProductNav",name:t.uniqueName,useSpan:!0},a.default.createElement("a",{"aria-current":t===r&&"page",href:t.isBlocked?"#":t.url,className:i,id:t.uniqueName,onClick:e.onClick(t),target:l,rel:"noopener"},t.displayName,u,t.isBlocked&&a.default.createElement(o.LockIcon,{className:"maestro-nav__lock"})),c&&c(t))))}))},t})(a.default.Component);t.ProductNav=i}),define("modules/clean/react/maestro_nav/shared_code/strings",["require","exports","tslib","modules/constants/maestro_nav_strings"],function(e,t,r,a){"use strict";function n(){return a.footerStrings}function o(){return a.homeAriaLabel}function s(){return a.navToggleAriaLabel}Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importStar(a),t.footerStrings=n,t.homeAriaLabel=o,t.navToggleAriaLabel=s}),define("modules/clean/react/maestro_nav/util/post_tti_components",["require","exports","tslib","react","modules/clean/react/maestro_nav/post_tti/api"],function(e,t,r,a,n){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importDefault(a),t.PostTTINewAdminCoachMark=n.PostNavTTIComponent({getPostTTIRenderer:function(e){var t=e.NewAdminCoachMark;return function(e){var r=e.activeAccountId,n=e.tooltipContent;return a.default.createElement(t,{activeAccountId:r,tooltipContent:n})}}}),t.PostTTILeftHA=n.PostNavTTIComponent({getPostTTIRenderer:function(e){var t=e.MaestroHA;return function(e){return a.default.createElement(t,{haProps:e})}}})}),define("modules/clean/upsell/prompt_event_emitter",["require","exports","tslib","eventemitter3"],function(e,t,r,a){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importDefault(a);var n={ON_HA_LOADED:"ON_HA_LOADED",REACT_HA_DID_UPDATE:"REACT_HA_DID_UPDATE",ON_PROMPT_INITIALIZED:"ON_PROMPT_INITIALIZED",ON_PROMPT_SHOWN:"ON_PROMPT_SHOWN",ON_PROMPT_DISMISSED:"ON_PROMPT_DISMISSED"};t.Events=n;var o=new a.default;t.eventEmitter=o;var s=(function(e){function t(){var t=e.call(this)||this;return t.allEventBuffers={},t}return r.__extends(t,e),t.prototype.on=function(t,r){var a=this;e.prototype.on.call(this,t,r);var n=this.allEventBuffers[t.toString()];return void 0!==n&&n.forEach(function(e){a.emit(t,e)}),this},t.prototype.emit=function(t,r){return t=t.toString(),void 0===this.allEventBuffers[t]&&(this.allEventBuffers[t]=[]),this.allEventBuffers[t].push(r),e.prototype.emit.call(this,t,r)},t.prototype.reset=function(){this.allEventBuffers={},this.removeAllListeners()},t})(a.default);t.BufferedEventEmitter=s,window.ensemble&&!window.ensemble.bufferedEventEmitter&&(window.ensemble.bufferedEventEmitter=new s);var i=window.ensemble&&window.ensemble.bufferedEventEmitter?window.ensemble.bufferedEventEmitter:new s;t.promptBufferedEventEmitter=i}),define("modules/clean/upsell/prompt_init",["require","exports","tslib","modules/clean/upsell/exception","modules/clean/upsell/prompt_event_emitter"],function(e,t,r,a,n){"use strict";function o(){return window.performance&&window.performance.mark&&window.performance.measure&&window.location.search.indexOf("show_debug_spans")>-1}function s(e){o()&&window.performance.mark("PromptInit start");try{var t=new i(e.prompt_loaded_status);n.promptBufferedEventEmitter.emit(n.Events.ON_PROMPT_INITIALIZED,t)}catch(t){a.reportException({err:t,tags:[a.TAGS.PROMPT_INIT],exc_extra:{param_config:e}})}}Object.defineProperty(t,"__esModule",{value:!0}),a=r.__importStar(a);var i=(function(){function e(e){this.prompt_loaded_status=e}return e.prototype.didHeaderBubbleLoad=function(){return!!this.prompt_loaded_status.HEADER_BUBBLE},e.prototype.didHomeModalLoad=function(){return!!this.prompt_loaded_status.HOME_MODAL==!0},e.prototype.didTargetedBubbleLoad=function(){return!!this.prompt_loaded_status.TARGETED_BUBBLE==!0},e.prototype.didHeaderLinkLoad=function(){return!!this.prompt_loaded_status.HEADER_LINK==!0},e.prototype.didCampaignForUserEducationLoad=function(){return!!this.prompt_loaded_status.ONBOARDING_MODULE},e.prototype.didMainCampaignLoad=function(){return this.didHeaderBubbleLoad()||this.didTargetedBubbleLoad()||this.didHomeModalLoad()},e.prototype.didAccountHeaderCampaignLoad=function(){return this.didHeaderBubbleLoad()||this.didHeaderLinkLoad()},e.prototype.didTopNotificationLoad=function(){return!!this.prompt_loaded_status.TOP_NOTIFICATION},e.prototype.didAnyContentLoad=function(){return this.didHeaderBubbleLoad()||this.didHomeModalLoad()||this.didTargetedBubbleLoad()||this.didHeaderLinkLoad()||this.didTopNotificationLoad()},e})();t.ResponseParser=i,t.initialize_module=s});
//# sourceMappingURL=pkg-maestro-nav.min.js-vflVsF9ju.map