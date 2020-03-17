define("modules/clean/defer",["require","exports"],function(e,t){"use strict";Object.defineProperty(t,"__esModule",{value:!0});var o=(function(){function e(e){var t=this;this.isResolved=!1,this.promise=new Promise(function(o,r){t.res=o,t.rej=r,e&&(t.isResolved=!0,o(e))})}return e.prototype.then=function(e,t){return this.promise.then(e,t)},e.prototype.catch=function(e){return this.promise.catch(e)},e.prototype.resolve=function(e){return this.isResolved=!0,this.res(e)},e.prototype.reject=function(e){return this.rej(e)},e})();t.Deferred=o}),define("modules/clean/deferred_loader",["require","exports","tslib","modules/clean/defer","external/lodash"],function(e,t,o,r,n){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),n=o.__importStar(n);var a=(function(){function e(){this.resources={}}return e.prototype.setItem=function(e,t){this.resources[e]?this.resources[e].resolve(t):this.resources[e]=new r.Deferred(t)},e.prototype.setItems=function(e){var t=this;Object.keys(e).map(function(o){return t.setItem(o,e[o])})},e.prototype.reset=function(){this.resources={}},e.prototype.resetItem=function(e){this.resources[e]&&this.resources[e].reject(new Error("item has been reset")),delete this.resources[e]},e.prototype.getItem=function(e,t){return this.resources[e]||(this.resources[e]=new r.Deferred,t&&t()),this.resources[e]},e.prototype.getResolvedItems=function(e,t){var o=this,r=e.map(function(e){return o.getItem(e)});return t&&n.some(r,function(e){return!e.isResolved})&&t(),Promise.all(r.map(function(e){return e.promise})).then(function(t){return t.reduce(function(t,o,r){var a;return n.assignIn(t,(a={},a[e[r]]=o,a))},{})})},e})(),s=window;s.deferredLoader||(s.deferredLoader=new a),t.deferredLoader=s.deferredLoader}),define("modules/clean/react/paginator",["require","exports","tslib","external/classnames","react","external/react-dom-factories","external/prop-types","external/lodash","modules/clean/react/sprite","modules/core/i18n"],function(e,t,o,r,n,a,s,i,l,c){"use strict";function p(e,t,o){for(var r=[],n=e<t,a=o?n?t+1:t-1:t,s=e;n?s<a:s>a;n?s++:s--)r.push(s);return r}Object.defineProperty(t,"__esModule",{value:!0}),r=o.__importDefault(r),n=o.__importDefault(n),a=o.__importStar(a),s=o.__importStar(s),i=o.__importStar(i);var u=(function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return o.__extends(t,e),t.prototype.render=function(){var e=this;return a.li({onClick:this.props.onClick},(function(){if(e.props.enabled){var t={"pagination-link":!0,active:e.props.active};return a.a({href:"#",className:r.default(t,e.props.optionalClass)},e.props.children)}return a.span({className:r.default("pagination-link disabled",e.props.optionalClass)},e.props.children)})())},t.displayName="PaginatorPageButton",t.propTypes={enabled:s.bool,active:s.bool,onClick:s.func,optionalClass:s.string},t.defaultProps={enabled:!0,active:!0,onClick:function(){},optionalClass:""},t})(n.default.Component),d=(function(e){function t(){var t=null!==e&&e.apply(this,arguments)||this;return t.state={first_page:0,active_page:0},t.need_arrows=function(){return t.props.isMaestroDesign||t.props.num_pages>t.props.max_visible_pages},t.left_button_enabled=function(){return t.need_arrows()&&t.state.active_page>0},t.right_button_enabled=function(){return t.need_arrows()&&t.state.active_page<t.props.num_pages-1},t.current_visible_pages=function(){return p(t.state.first_page,i.min([t.props.num_pages,t.state.first_page+t.props.max_visible_pages]),!1)},t.ensure_in_range=function(e,t,o){return Math.max(t,Math.min(o,e))},t.setActivePage=function(e){e=t.ensure_in_range(e,0,t.props.num_pages-1);var o=e-Math.floor(t.props.max_visible_pages/2);o=t.ensure_in_range(o,0,t.props.num_pages-t.props.max_visible_pages);var r=o+t.props.max_visible_pages;e!==t.state.active_page&&"function"==typeof t.props.onChangePage&&t.props.onChangePage(e),t.setState({first_page:o,last_page:r,active_page:e})},t.left_clicked=function(e){return e.preventDefault(),t.setActivePage(t.state.active_page-1)},t.right_clicked=function(e){return e.preventDefault(),t.setActivePage(t.state.active_page+1)},t.page_clicked=function(e,o){return o.preventDefault(),t.setActivePage(e)},t}return o.__extends(t,e),t.prototype.render=function(){var e,t,o=this;return this.props.isMaestroDesign?(e=n.default.createElement(l.Sprite,{group:"web",name:"chevron",alt:c._("left-arrow"),className:"arrow-left-sprite"}),t=n.default.createElement(l.Sprite,{group:"web",name:"chevron",alt:c._("right-arrow")})):(e=a.span({className:"arrow arrow-left"},c._("Previous")),t=a.span({className:"arrow arrow-right"},c._("Next"))),a.div({className:"nav nav-inline"},a.ol({className:r.default("pagination",this.props.optionalClass)},this.need_arrows()?n.default.createElement(u,{optionalClass:"arrow-left-container",enabled:this.left_button_enabled(),onClick:this.left_clicked},e):void 0,Array.from(this.current_visible_pages()).map(function(e){return n.default.createElement(u,{active:e===o.state.active_page,onClick:o.page_clicked.bind(o,e),key:e},""+(e+1))}),this.need_arrows()?n.default.createElement(u,{optionalClass:"arrow-right-container",enabled:this.right_button_enabled(),onClick:this.right_clicked},t):void 0))},t.displayName="Paginator",t.propTypes={max_visible_pages:s.number,onChangePage:s.func,num_pages:s.number,isMaestroDesign:s.bool,optionalClass:s.string},t.defaultProps={max_visible_pages:9,num_pages:1,isMaestroDesign:!1},t})(n.default.Component);t.default=d}),define("modules/clean/react/table",["require","exports","tslib","external/classnames","lru","react","external/react-dom-factories","external/prop-types","jquery","modules/clean/react/paginator","modules/clean/react/sprite","modules/clean/react/tooltip","modules/core/i18n"],function(e,t,o,r,n,a,s,i,l,c,p,u,d){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),r=o.__importDefault(r),n=o.__importDefault(n),a=o.__importDefault(a),s=o.__importStar(s),i=o.__importStar(i),l=o.__importDefault(l),c=o.__importDefault(c);var f=function(){for(var e=[],t=0;t<arguments.length;t++)e[t]=arguments[t];return r.default(l.default.extend.apply(l.default,[{}].concat(Array.from(e))))},m=(function(e){function t(t){var o=e.call(this,t)||this;o.forceRefresh=function(){return o.cache.removeAll(),o.props.infiniteScroll?(window.scrollTo(0,0),o.refreshData(!0)):o.setState({rows:[]},o.refreshData)},o.onInitialDataLoaded=function(e){var t;return o.setState({loading:!1,total:null!=(null!=e?e.total:void 0)?null!=e?e.total:void 0:0,rows:null!=(t=o.props.infiniteScroll?null!=e?e.rows:void 0:null!=e?e.rows.slice(0,o.props.rowsPerPage):void 0)?t:[]})},o.updateRows=function(e){return o.setState({rows:e(o.state.rows)})},o.refreshData=function(e){void 0===e&&(e=!1);var t,r;if(!(o.props.infiniteScroll&&o.state.rows.length>=o.state.total&&0!==o.state.total)||e){var n=e?[]:o.state.rows;o.setState({loading:!0}),r=o.props.infiniteScroll?{offset:n.length,limit:o.props.rowsPerRequestForInfiniteScroll,sortColumn:null!=o.props.columns[o.state.sortColumn]?o.props.columns[o.state.sortColumn].field:void 0,sortDir:o.state.sortDir}:{offset:o.state.currentPage*o.props.rowsPerPage,limit:o.props.rowsPerPage,sortColumn:null!=o.props.columns[o.state.sortColumn]?o.props.columns[o.state.sortColumn].field:void 0,sortDir:o.state.sortDir};var a=JSON.stringify(r);if(t=o.cache.get(a))return o.props.infiniteScroll?o.setState({rows:n.concat(t.rows),total:t.total}):o.setState(t),void o.setState({loading:!1});null==o.requestId&&(o.requestId=0);var s=++o.requestId;return o.props.onRequestData(r,function(e,t,r){if(null==e&&o.requestId===s){var i={rows:t.rows,total:t.total};return o.cache.put(a,i),"function"==typeof o.props.onTotalUpdated&&o.props.onTotalUpdated(i.total),o.props.infiniteScroll?o.setState({rows:n.concat(i.rows),total:i.total}):o.setState(i),(null!=r?r.displayLoading:void 0)?void 0:o.setState({loading:!1})}})}},o.onColumnHeaderClick=function(e){if(o.props.columns[e].sortable!==!1){var t=e===o.state.sortColumn?{sortDir:-o.state.sortDir}:{sortColumn:e,sortDir:null!=o.props.columns[e].initialSortDir?o.props.columns[e].initialSortDir:1};return o.props.infiniteScroll&&(t.rows=[]),o.setState(t,function(){return o.refreshData()})}},o.onChangePage=function(e){return o.setState({currentPage:e},o.refreshData)},o.onScroll=function(e){if(o.props.infiniteScroll)return window.innerHeight+Math.ceil(window.pageYOffset)>=document.body.offsetHeight?o.refreshData():void 0},o.renderStaticRow=function(){var e=o.props.staticRow;return e.showOnAllPages||0===o.state.currentPage?s.li({className:f(e.rowClass,{"table-row":!0}),key:"static-row-0"},s.span({className:f(o.props.cellClasses,{"table-cell":!0})},e.render())):null};var r;return o.state={total:null!=(null!=t.initialData?t.initialData.total:void 0)?null!=t.initialData?t.initialData.total:void 0:0,rows:null!=(r=t.infiniteScroll?null!=t.initialData?t.initialData.rows:void 0:null!=t.initialData?t.initialData.rows.slice(0,t.rowsPerPage):void 0)?r:[],currentPage:0,sortColumn:t.initialSortColumn,sortDir:t.initialSortDir,loading:t.initialDataLoading},o}return o.__extends(t,e),t.prototype.componentWillMount=function(){if(this.cache=new n.default(1e3),!(null!=this.props.initialData?this.props.initialData.rows:void 0)&&!this.state.loading)return this.refreshData()},t.prototype.componentDidMount=function(){if(this.props.infiniteScroll&&window.addEventListener("scroll",this.onScroll),this.props.handleTableUpdated&&!this.state.loading)return this.props.handleTableUpdated()},t.prototype.componentDidUpdate=function(){if(this.props.handleTableUpdated&&!this.state.loading)return this.props.handleTableUpdated()},t.prototype.componentWillUnmount=function(){if(this.props.infiniteScroll)return window.removeEventListener("scroll",this.onScroll)},t.prototype.render=function(){var e,t,o=this,r=0===this.state.total&&!this.state.loading;return s.div({className:f(this.props.classes,{"table-container":!0})},r&&this.props.hideHeaderOnEmptyTable?void 0:s.div({className:f(this.props.headerClasses,{"table-header":!0})},(function(){var r=[];for(e=0;e<o.props.columns.length;e++)t=o.props.columns[e],r.push(s.div({className:f(t.classes,o.props.cellClasses,{"table-column-header":!0,nonsortable:t.sortable===!1,sorted:t.sortable!==!1&&o.state.sortColumn===e}),style:{width:t.width},onClick:o.onColumnHeaderClick.bind(o,e),key:e},t.header,(function(){if(t.sortable!==!1&&o.state.sortColumn===e)switch(o.state.sortDir){case 1:return s.span({className:"sort-arrow sort-arrow-up"});case-1:return s.span({className:"sort-arrow sort-arrow-down"})}})(),t.tooltip?s.div({className:"table-column-header-info-tooltip"},a.default.createElement(u.Tooltip,{position:u.TooltipPosition.RIGHT,tooltip_contents:t.tooltip,hide_delay:250},a.default.createElement(p.Sprite,{group:"web",name:"info",alt:"info tooltip"}))):void 0));return r})()),r?s.div({className:"empty-message"},this.props.emptyMessage):this.state.rows.length>0?s.ol({className:f(this.props.bodyClasses,{"table-body":!0,loading:this.state.loading})},this.props.staticRow?this.renderStaticRow():void 0,(function(){for(var r=[],n=function(n){var a=void 0,i=o.state.rows[n];a="function"==typeof o.props.rowClasses?o.props.rowClasses(i):o.props.rowClasses,r.push(s.li({className:f(a,{"table-row":!0}),key:n,onClick:null!=o.props.onRowClicked?o.props.onRowClicked.bind(o,i):void 0},(function(){var r=[];for(e=0;e<o.props.columns.length;e++){var n=void 0,a=void 0;t=o.props.columns[e],r.push(s.span({className:f(t.classes,o.props.cellClasses,{"table-cell":!0}),style:{width:t.width},key:e},null!=(n=null!=(a="function"==typeof t.render?t.render(i):void 0)?a:i[t.field])?n:s.span({className:"null-field"},"—")))}return r})()))},a=0;a<o.state.rows.length;a++)n(a);return r})()):s.div({className:"spacer"}),this.state.loading?s.div({className:"loading-message"},this.props.loadingIndicator?s.img({src:this.props.loadingIndicator,alt:d._("Loading...")}):d._("Loading...")):void 0,(function(){if(!o.props.infiniteScroll&&o.state.total>o.props.rowsPerPage){var e=a.default.createElement(c.default,{num_pages:Math.ceil(o.state.total/o.props.rowsPerPage),onChangePage:o.onChangePage,isMaestroDesign:o.props.isMaestroDesign,optionalClass:o.props.paginatorClasses});return o.props.isMaestroDesign?s.div({className:"pagination-container"},e):e}})())},t.displayName="Table",t.propTypes={columns:i.arrayOf(i.shape({field:i.string,header:i.node,width:i.oneOfType([i.number,i.string]),sortable:i.bool,initialSortDir:i.number,tooltip:i.string,classes:i.object,render:i.func})),handleTableUpdated:i.func,onRequestData:i.func,onTotalUpdated:i.func,initialData:i.shape({total:i.number,rows:i.array}),initialDataLoading:i.bool,emptyMessage:i.node,classes:i.object,headerClasses:i.object,bodyClasses:i.object,rowClasses:i.oneOfType([i.object,i.func]),cellClasses:i.object,paginatorClasses:i.string,initialSortColumn:i.number,initialSortDir:i.number,rowsPerPage:i.number,infiniteScroll:i.bool,rowsPerRequestForInfiniteScroll:i.number,loadingIndicator:i.string,onRowClicked:i.func,hideHeaderOnEmptyTable:i.bool,isMaestroDesign:i.bool,staticRow:i.shape({field:i.string,rowClass:i.string,render:i.func,showOnAllPages:i.bool})},t.ASC_DIR=1,t.DESC_DIR=-1,t.defaultProps={initialSortColumn:0,initialSortDir:1,rowsPerPage:10,hideHeaderOnEmptyTable:!1},t})(a.default.Component);t.default=m}),define("modules/clean/teams/admin/maestro/admin_console_constants",["require","exports"],function(e,t){"use strict";Object.defineProperty(t,"__esModule",{value:!0});(function(e){e[e.Admin=0]="Admin",e[e.Action=1]="Action"})(t.IconGroup||(t.IconGroup={}))}),define("modules/clean/teams/coach_mark_tooltip",["require","exports","tslib","external/classnames","react","external/react-dom","spectrum/icon_form","spectrum/button","modules/clean/react/css","modules/clean/react/portal","modules/clean/user_education/user_education_interface","modules/core/i18n"],function(e,t,o,r,n,a,s,i,l,c,p,u){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),r=o.__importDefault(r),n=o.__importDefault(n),a=o.__importStar(a);var d=(function(e){function t(t){var o=e.call(this,t)||this;return o.onTransitionEnd=function(){var e=o.state,t=e.willBeClosed,r=e.willBeShown;t?o.closeTooltip():r&&o.showTooltip()},o.onCtaClick=function(){(0,o.props.onCtaClick)(),o.willCloseTooltip()},o.willCloseTooltip=function(){var e=o.props.onWillClose;o.setState({isShown:!1,willBeClosed:!0},function(){e()})},o.closeTooltip=function(){var e=o.props.onDidClose;o.setState({isClosed:!0,willBeClosed:!1},function(){e()})},o.willShowTooltip=function(){var e=o.props.onWillShow;o.setState({willBeShown:!0},function(){e()})},o.showTooltip=function(){var e=o.props.onDidShow;o.setState({isShown:!0,willBeShown:!1},function(){e()})},o.setInlineStyle=function(){var e=o.state.isShown,t=a.findDOMNode(o.triggerEl).getBoundingClientRect();o.tooltipEl&&(o.tooltipEl.style.left=e?o.getOffsetX(t)+"px":"0px",o.tooltipEl.style.position="fixed",o.tooltipEl.style.top=e?o.getOffsetY(t)+"px":"-1000px",o.tooltipEl.style.zIndex="10")},o.getOffsetX=function(e){var t=o.props,r=t.offsetX,n=t.position,a=0;switch(n){case p.UEEffectPosition.LeftCenter:a=0;break;case p.UEEffectPosition.RightCenter:a=e.width-398}return e.left+a+r},o.getOffsetY=function(e){var t=o.props.offsetY;return e.top+e.height+t+10},o.state={isClosed:!1,isShown:!1,willBeClosed:!1,willBeShown:!1},o}return o.__extends(t,e),t.prototype.componentDidMount=function(){window.addEventListener("resize",this.setInlineStyle),this.willShowTooltip()},t.prototype.componentDidUpdate=function(){this.setInlineStyle()},t.prototype.componentWillUnmount=function(){window.removeEventListener("resize",this.setInlineStyle)},t.prototype.render=function(){var e=this,t=this.props,o=t.children,a=t.ctaText,l=t.hasCloseButton,d=t.id,f=t.info,m=t.pointerPosition,h=t.subtitle,_=t.title,g=this.state,v=g.isClosed,w=g.isShown,C=g.willBeClosed,b=g.willBeShown,D=r.default("coach-mark-tooltip",{"coach-mark-tooltip--pointer-top-right":m===p.UEToolTipPointerPosition.TopRight,"coach-mark-tooltip--enter":b||w,"coach-mark-tooltip--enter-active":w,"coach-mark-tooltip--leave":C||v,"coach-mark-tooltip--leave-active":v});return n.default.createElement("div",null,n.default.createElement("div",{"aria-describedby":d,ref:function(t){e.triggerEl=t}},n.default.Children.only(o)),n.default.createElement(c.Portal,{className:"coach-mark-tooltip__portal",parentElement:document.body},n.default.createElement("div",{"aria-hidden":!w,className:D,id:d,onTransitionEnd:this.onTransitionEnd,ref:function(t){e.tooltipEl=t},role:"tooltip"},n.default.createElement("div",{className:"coach-mark-tooltip__content"},l&&n.default.createElement(i.Button,{"aria-label":"Close",className:"coach-mark-tooltip__close",onClick:this.willCloseTooltip,variant:"circular"},n.default.createElement(s.IconForm,{name:"cancel"})),n.default.createElement("div",{className:"coach-mark-tooltip__title"},_),n.default.createElement("div",{className:"coach-mark-tooltip__subtitle"},h)),n.default.createElement("div",{className:"coach-mark-tooltip__footer"},n.default.createElement("div",{className:"coach-mark-tooltip__info"},f),n.default.createElement(i.Button,{className:"coach-mark-tooltip__cta",onClick:this.onCtaClick,variant:"borderless"},a||u._("Got it"))))))},t.displayName="CoachMarkTooltip",t.defaultProps={offsetX:0,offsetY:0,onCtaClick:function(){},onDidClose:function(){},onDidShow:function(){},onWillClose:function(){},onWillShow:function(){},position:p.UEEffectPosition.LeftCenter,showCloseButton:!1},t})(n.default.Component);t.CoachMarkTooltip=l.requireCssWithComponent(d,["/static/css/teams/coach_mark_tooltip-vflK5GeeE.css"])}),define("modules/clean/teams/constants_team",["require","exports"],function(e,t){"use strict";Object.defineProperty(t,"__esModule",{value:!0}),t.JOIN_STATES={ACTIVE:1,PENDING:2,ERROR:3,REMOVED:4,SUSPENDED:5},t.REMOVE_TYPES={DELETED:2,OFFBOARDED:3},t.USER_TYPES={PENDING_TEAM_ADMIN:11100,PENDING_ACCOUNTS_ADMIN:11200,PENDING_SUPPORT_ADMIN:11300,PENDING_MEMBER:12e3,TEAM_ADMIN:21100,ACCOUNTS_ADMIN:21200,SUPPORT_ADMIN:21300,MEMBER:22e3,SUSPENDED:3e4,REMOVED:4e4},t.ADMIN_ONBOARDING_STATES={NONE:0,UPLOAD_MODAL:1,TEAM_TAB_TOOLTIP:2,INVITATION_TOOLTIP:3,GROUP_TAB_TOOLTIP:4,SETTINGS_TOOLTIP:5,PAPER_TAB_TOOLTIP:6,DONE:1e3,UPLOADED_FILE:1001},t.JOIN_REQUEST_STATUS={PENDING:1,APPROVED:2,REJECTED:3}});
//# sourceMappingURL=pkg-team-utils.min.js-vfl63w8tQ.map