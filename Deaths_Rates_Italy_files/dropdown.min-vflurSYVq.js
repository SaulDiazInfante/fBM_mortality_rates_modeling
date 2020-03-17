define(["require","exports","tslib","react-redux","react","modules/clean/browse_uri_interface","modules/clean/filepath","modules/clean/react/browse/models","modules/clean/react/css","modules/clean/search/constants","modules/clean/search/search_bar/dropdown","modules/clean/search/single_page/data/action_creators","modules/clean/search/single_page/data/selectors/index","modules/clean/search/single_page/logger","modules/clean/search/single_page/utils","modules/clean/search/types","modules/core/browser","modules/clean/web_user_action_events"],function(e,r,s,o,t,n,a,i,l,c,h,u,p,d,g,y,_,m){"use strict";Object.defineProperty(r,"__esModule",{value:!0}),t=s.__importStar(t),u=s.__importStar(u),p=s.__importStar(p),d=s.__importStar(d);var S=(function(e){function r(){var r=null!==e&&e.apply(this,arguments)||this;return r.focusInputBox=function(){r.props.inputBox&&r.props.inputBox.current&&r.props.inputBox.current.focus()},r.handleSearch=function(){r.props.searchUrl&&_.redirect(r.props.searchUrl)},r.handleHistoryClearAll=function(){r.props.clearHistory(),d.logHistoryCleared(),r.focusInputBox()},r.handleHistoryClick=function(e,s){r.props.setQuery({query:e.normalizedQuery}),d.logHistoryClicked(e.normalizedQuery,s),r.focusInputBox()},r.handleResultObjectPathClick=function(e){if(e instanceof i.File){var s=n.browse_uri_for_fq_path(r.props.user,a.parent_dir(e.fq_path)).toString();_.redirect(s)}},r.handleHighlightRow=function(e){r.props.setSuggestionHighlightRow({index:e})},r.handleDropdownTTI=function(){},r.handleSearchResultOpen=function(e){g.navigateToResult(e,r.props.user,r.props.results),d.logNavigateToResult(r.props.results,e,m.ActionSurfaceLogValue.TOP_NAVIGATION,m.ActionSourceValue.ROW_SINGLE_CLICK)},r}return s.__extends(r,e),r.prototype.componentDidMount=function(){this.props.loadHistory()},r.prototype.render=function(){return this.props.user?t.createElement(h.SearchBarDropdown,{isOpen:!!this.props.query||!!this.props.queryHistory.length,onHistoryClearAllClick:this.handleHistoryClearAll,onHistoryItemClick:this.handleHistoryClick,onSearchResultOpen:this.handleSearchResultOpen,onResultObjectPathClick:this.handleResultObjectPathClick,onSearch:this.handleSearch,onHighlightDropdownItem:this.handleHighlightRow,searchPath:"",normalizedQuery:this.props.query,searchResults:this.props.results.toArray(),searchBarLoading:this.props.loadingState===c.SearchLoadingState.LOADING_FIRST_PAGE,showLinkToResultsPage:""!==this.props.query,highlightedRow:this.props.highlightedRow,user:this.props.user,searchSessionId:this.props.sessionId,onDropdownTTI:this.handleDropdownTTI,searchHistory:this.props.queryHistory,dropdownHeaderItems:[y.SearchBarDropdownHeaderItemType.SEARCH_ALL],searchBarExperiments:{},browseScope:"",className:"search__dropdown"}):null},r})(t.PureComponent);r.SearchDropdownComponent=S;var w=function(e){return{user:p.user(e),loadingState:p.loadingState(e),query:p.resultQuery(e),results:p.resultsList(e),searchUrl:p.searchUrl(e),sessionId:p.sessionId(e),queryHistory:p.queryHistory(e),highlightedRow:p.highlightedRow(e)}},f={setQuery:u.setQuery,clearHistory:u.clearAllHistory,loadHistory:u.loadHistory,setSuggestionHighlightRow:u.setHighlightedRow};r.ConnectedSearchDropdown=o.connect(w,f)(S),r.SearchDropdown=l.requireCssWithComponent(r.ConnectedSearchDropdown,["/static/css/search/search_bar_react-vflmaJPTU.css"])});
//# sourceMappingURL=dropdown.min.js-vflt7Aluj.map