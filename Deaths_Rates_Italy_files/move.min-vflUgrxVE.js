define(["require","exports","tslib","external/immutable","external/lodash","modules/clean/logging/hive/schemas/web-modal-activity","modules/clean/react/async_fileops_modals","modules/clean/react/browse/actions","modules/clean/react/browse/action_logger","modules/clean/react/browse/data/action_creators","modules/clean/react/browse/data/selectors","modules/clean/react/browse/data/store","modules/clean/react/browse/logger_util","modules/clean/react/fileops_actions","modules/clean/react/folder_dialog/api","modules/clean/react/folder_dialog/constants","modules/clean/react/selection","modules/clean/search/single_page/data/action_creators","modules/clean/search/single_page/data/store","modules/clean/web_user_action_events","modules/core/i18n","modules/clean/react/async_overquota_modal_controller","modules/clean/react/browse/file_actions/file_action_helpers"],function(e,t,o,s,r,n,a,l,i,c,u,d,_,m,p,f,g,v,h,S,F,w,b){"use strict";function O(){w.showOverquota("wb_oq_mv_cp")}function M(e,t){var o=t.isOverFreeQuota,s=t.hasExcludedNsIds,r=t.moveItemDisabled,n=o&&!s?O:y(e);return{label:F._("Move"),performAction:n,disabled:r,iconName:"move",className:"action-move",key:"move"}}Object.defineProperty(t,"__esModule",{value:!0}),s=o.__importStar(s),r=o.__importStar(r),a=o.__importStar(a),i=o.__importStar(i),u=o.__importStar(u),_=o.__importStar(_),m=o.__importStar(m),w=o.__importStar(w);var y=function(e){return function(){var t=e.user,F=e.context,w=e.files,O=e.firstFile,M=e.source,y=e.isSearchMode,A=e.viewType,q=e.useSearchSuccessBanner,E=e.expBrowseSuccessBanner,P=e.expListSubfoldersWithOdyssey,N=e.expMoveUseApiV2,x=w.toArray();b.logUserEventOnFile({result:O,args:e,action_type:S.WebUserActionLogEvent.OPEN_MOVE_MODAL}),b.showRetrievalSuccessBanner(q,y,E,S.WebUserActionLogEvent.OPEN_MOVE_MODAL);var B={user:t,files:x,path:y?void 0:F.currentFQPath,onSuccess:function(e,o){if(y&&o){var r=h.getStoreForSearch();r.dispatch(v.updateResults({results:o})),r.dispatch(v.setSelection({selection:new g.Selection({selected:s.OrderedSet(o.map(function(e){return e.fq_path})),anchor:null}),skipLogging:!0}))}else if(o){var r=d.getStoreForBrowse(),n=u.unsortedFiles(r.getState());o.forEach(function(e){var t=n.find(function(t){return t.file_id===e.file_id});t&&(n=n.remove(t.fq_path))}),r.dispatch(c.setUnsortedFiles({unsortedFiles:n}))}var a=_.countFilesAndFolders(w),l=a.num_files_selected,m=a.num_folders_selected;i.logMoveFiles({uid:t.id,source:M,view_type:A,result:O,num_files_selected:l,num_folders_selected:m})},onRequestNotSent:function(){l.browseActions.selectFilesByFqPath({fqPathsToSelect:x.map(function(e){return e.fq_path})})},onUndo:function(e){if(y&&e){var t=h.getStoreForSearch();t.dispatch(v.updateResults({results:x})),t.dispatch(v.setSelection({selection:new g.Selection({selected:s.OrderedSet(x.map(function(e){return e.fq_path})),anchor:null}),skipLogging:!0}))}l.browseActions.selectRowsFromBoltClient(x)}},L=P?p.getFolderContentsAPIv2:p.getFolderContents;a.showMoveDialog({operation:f.Operation.MOVE,modalName:n.ModalName.MOVE,user:t,apiClient:{moveToFolder:function(e){var t=o.__assign({},B,{newPath:e,checkFSWs:!0,useApiV2:N});return m.moveFiles(t),Promise.resolve()},getFolderContents:L,getFolderSuggestion:p.getFolderSuggestion},entries:x,initialFolderSelection:F.currentFQPath,onSuccess:r.noop,onCancel:r.noop,showFolderSuggestion:!0,createNewFolderInMoveDialog:!0})}};t.getMoveItem=M});
//# sourceMappingURL=move.min.js-vflA3Jui0.map