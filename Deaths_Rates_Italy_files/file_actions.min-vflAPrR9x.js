define(["require","exports","tslib","react","modules/clean/browse_interface","modules/clean/cloud_docs/constants","modules/clean/em_string","modules/clean/filepath","modules/clean/filetypes","modules/clean/react/app_actions/app_actions_menu","modules/clean/react/async_file_modal_controller","modules/clean/react/badge","modules/clean/react/browse/actions","modules/clean/react/browse/client_download_modal","modules/clean/react/browse/constants","modules/clean/react/browse/data/folder_overview_selectors","modules/clean/react/browse/file_actions/actions/copy","modules/clean/react/browse/file_actions/actions/move","modules/clean/react/browse/file_actions/actions/share","modules/clean/react/browse/file_actions/file_action_helpers","modules/clean/react/browse/models","modules/clean/react/delorean/data/types","modules/clean/react/extensions/data/types","modules/clean/react/extensions/utils","modules/clean/react/file_lock/api","modules/clean/react/file_transfers/async/browse_entry_point_onboarding_modal_async","modules/clean/react/file_transfers/lib/api","modules/clean/react/file_transfers/lib/urls","modules/clean/react/file_transfers/lib/utils","modules/clean/react/files_view/types","modules/clean/react/folder_overview/async_components","modules/clean/react/folder_overview/data/action_creators","modules/clean/react/folder_overview/data/store","modules/clean/react/folder_overview/utils","modules/clean/react/retrieval_success_banner/util","modules/clean/react/snackbar","modules/clean/react/starred/browse_exports","modules/clean/search/types","modules/clean/sharing/browse_exports","modules/clean/sharing/constants","modules/clean/sharing/gating_util","modules/clean/unity/check_file_cache","modules/clean/unity/features","modules/clean/viewer","modules/clean/web_user_action_events","modules/core/browser","modules/core/i18n"],function(e,r,n,o,s,i,t,a,l,c,d,u,f,h,p,m,w,_,v,S,F,g,y,E,b,A,T,U,O,R,N,L,I,k,B,D,M,C,P,x,V,W,q,z,H,Q,G){"use strict";function K(r){return n.__awaiter(this,void 0,Promise,function(){var o;return n.__generator(this,function(s){switch(s.label){case 0:return[4,new Promise(function(r,n){e(["modules/clean/sharing/content_manager"],r,n)}).then(n.__importStar)];case 1:return o=s.sent().goToContentManagerForPath,o(r),[2]}})})}function J(e){return e.isDeleted||e.is_unmounted}function j(e){var r=e.toArray().map(function(e){return e.fq_path});return a.inSameDirectory(r)}function X(e){var r=e.context,o=e.files,s=e.user,i=e.viewType,t=e.sharingServiceInfo,a=e.extensionsFeatureFlags,l=e.canDisplayFolderSizes,c=void 0!==l&&l,d=e.isSearchMode,u=void 0!==d&&d,f=e.isQueryingFolderSizes,h=e.expFolderHistoryRollbacks,p=void 0!==h&&h,m=e.pinnedItems,w=e.expRewindUpsell,_=e.showAddToTransfer,v=e.showAddFolderToTransfer,S=e.expShowFileLocking,F=void 0!==S&&S,g=n.__assign({},re);if(r.isInsideArchivedTeamFolder)return g.showDownload=!0,g;var y=o.every(function(e){return!J(e)}),E=!y&&o.every(function(e){return J(e)});return y?Y({actions:g,canDisplayFolderSizes:c,expFolderHistoryRollbacks:p,context:r,files:o,sharingServiceInfo:t,extensionsFeatureFlags:a,isQueryingFolderSizes:f,isSearchMode:u,user:s,viewType:i,pinnedItems:m,expRewindUpsell:w,showAddToTransfer:_,showAddFolderToTransfer:v,expShowFileLocking:F}):E?Z({actions:g,files:o,context:r,user:s}):g}function Y(e){var r,n=e.actions,o=e.context,s=e.files,t=e.user,c=e.viewType,d=e.sharingServiceInfo,u=e.extensionsFeatureFlags,f=e.canDisplayFolderSizes,h=void 0!==f&&f,p=e.isQueryingFolderSizes,m=void 0!==p&&p,w=e.isSearchMode,_=void 0!==w&&w,v=e.expFolderHistoryRollbacks,S=void 0!==v&&v,y=e.pinnedItems,b=e.expRewindUpsell,A=void 0===b?g.UpsellVariant.OFF:b,T=e.showAddToTransfer,U=void 0!==T&&T,O=e.showAddFolderToTransfer,N=void 0!==O&&O,L=e.expShowFileLocking,I=void 0!==L&&L,k=s.first(),B=1===s.count(),D=B&&!k.is_dir,C=B&&k.is_dir,x=s.every(function(e){return e.type===l.FileTypes.FILE}),H=s.some(function(e){return e.type===l.FileTypes.TEAM_SHARED_FOLDER}),Q=s.some(function(e){return e.type===l.FileTypes.TEAM_MEMBER_FOLDER}),G=s.some(function(e){return e.type===l.FileTypes.SANDBOX}),K=s.some(function(e){return e instanceof F.FileSharedWithMe}),J=s.some(function(e){return e.is_cloud_doc}),X=s.some(function(e){return e.is_cloud_doc&&e instanceof F.FileSharedWithMe}),Y=!1;if(K||H||Q||(n.showCopy=!0,n.showDelete=!0,n.showMove=!0,n.showRename=B),!K&&H&&C&&t&&V.GoldenGate.hasContentManagerAndSuperAdmin(t.id)&&(n.showRenameTeamFolder=!0,n.showArchive=!0),!K&&!z.Viewer.get_viewer().is_assume_user_session&&t&&V.GoldenGate.hasContentManagerAndSuperAdmin(t.id)&&V.GoldenGate.hasNewOwnershipModel(t.id)&&o.isInsideTeamFolderTree&&(n.showManageAsAdmin=!0),B){var Z=s.first();r=P.shouldUsePublicFolderSharing(o.config,Z.fq_path),K||r||Q||G||(n.showTiburonShare=!0),Y=!K&&q.UnityFeatures.isUnitySupported()&&null!=W.default&&W.default.is_cached_and_locally_available(Z.ns_id,Z.ns_path),Y&&(n.showOpen=!0),K||(n.showOpenWith=!0),!K&&u&&d&&E.shouldShowSplitShare(u,d,Z)&&(n.showSplitShare=!0)}if(!K&&C&&(S||A!==g.UpsellVariant.OFF&&A!==g.UpsellVariant.CONTROL)&&(n.showRewindFolder=!0),n.showShowInFolder=!K&&_&&j(s),D&&(n.showComment=!k.is_cloud_doc,n.showVersions=!(K||k.is_cloud_doc&&i.isPointerByExtension(k.ns_path)),o.isInFolderMode?n.showCopyPublicFolderFileLink=!!r:n.showCopyPublicFolderFileLink=!1),D)if(k.is_locked)k.is_lockholder?n.showFileUnlock=!0:n.showFileRequestUnlock=!0;else if(I){var $=k.ns_id!==t.home_ns_id,ee=k.read_only||k.is_symlink||!$;n.showFileLock=!ee}if(Y||(n.showDownload=D?!J:!K&&!X&&(o.isInFolderMode||_)),n.showAddToTransfer=U&&!J&&(x||N)&&!s.some(F.File.isNoAccessSharedFolder),n.showRequestFiles=C&&o.isCurrentPathWriteable()&&!F.File.isReadOnlySharedFolder(k),!K&&B){var re=s.first().type;re!==l.FileTypes.SHARED_FOLDER&&re!==l.FileTypes.TEAM_SHARED_FOLDER||(n.showSharedFolderEvents=!0),C&&null==s.first().fetchFolderSizesStatus&&!m&&!_&&c!==R.ViewType.Grid&&h&&(n.showCalculateFolderSize=!0)}if(!K){var ne=s.every(function(e){return null==e.fetchFolderSizesStatus}),oe=s.every(function(e){return e.is_dir}),se=s.some(function(e){return!F.File.canViewFileMembers(e,o)});ne&&oe&&!se&&!m&&!_&&c!==R.ViewType.Grid&&h&&(n.showCalculateFolderSize=!0)}if(!K&&B){var ie={id:s.first().file_id,type:M.HOME_RESOURCE_ID_TYPE.ENCODED_FILE_OBJ_ID},te=M.StarredStore.getIsLoading(ie),ae=M.StarredStore.getIsStarred(ie);n.showStar=ae===!1,n.showUnstar=ae===!0,n.showLoadingStar=te}if(!K&&void 0!==y&&!_){var le=s.toArray().filter(function(e){return y.some(function(r){return r.fq_path===e.fq_path||r.file_id===e.file_id})}).length;o.isCurrentPathWriteable()&&(n.showUnpin=le===s.size,n.showPin=le<s.size),n.showPinTo=a.normalize(o.currentFQPath).split("/").length>1}return n}function Z(e){var r=e.actions,n=e.files,o=e.context,s=e.user,i=n.first();if(n.some(function(e){return e.is_unmounted||e.ns_id!==i.ns_id}))return r;1===n.count()&&i.is_dir&&o.inactiveNSIDByFQPath[i.fq_path.toLowerCase()]&&!o.isInsideTeamFolderTree?r.showReadd=!0:r.showRestore=!0;var t;if(o.isInFolderMode)t=o.permanentDeletionDisabledStateByNSID[o.currentNSID];else{t=n.map(function(e){return e.ns_id}).some(function(e){return o.permanentDeletionDisabledStateByNSID[e]})}return!z.Viewer.get_viewer().is_assume_user_session&&s&&V.GoldenGate.hasContentManagerAndSuperAdmin(s.id)&&V.GoldenGate.hasNewOwnershipModel(s.id)&&o.isInsideTeamFolderTree&&(r.showManageAsAdmin=!0),t||(r.showPermanentDelete=!0),r}function $(e){return function(){var r=e.firstFile,n=r.ns_id,o=r.ns_path;Q.redirect("/requests?ns_id="+n+"&path="+o+"&referrer=web_browse_file_action")}}function ee(e){var r=e.files,n=e.context,s=e.user,i=e.responsive,l=e.sharingServiceInfo,d=e.extensionsFeatureFlags,f=e.canDisplayFolderSizes,h=void 0!==f&&f,p=e.actionHandlers,m=e.skipSharing,S=void 0!==m&&m,E=e.source,b=e.viewType,A=e.isOverFreeQuota,T=void 0!==A&&A,U=e.hasExcludedNsIds,O=void 0!==U&&U,R=e.isSearchMode,N=void 0!==R&&R,L=e.isQueryingFolderSizes,I=void 0!==L&&L,k=e.canShowUJPopup,B=void 0!==k&&k,D=e.isSidebar,M=void 0!==D&&D,P=e.useSearchSuccessBanner,x=void 0!==P&&P,V=e.expBrowseSuccessBanner,W=void 0===V?C.BrowseSuccessBannerVariants.OFF:V,q=e.expListSubfoldersWithOdyssey,z=void 0!==q&&q,H=e.pinnedItems,Q=e.expFolderHistoryRollbacks,K=void 0!==Q&&Q,J=e.expRewindUpsell,j=void 0===J?g.UpsellVariant.OFF:J,Y=e.showAddToTransfer,Z=void 0!==Y&&Y,ee=e.showAddFolderToTransfer,re=void 0!==ee&&ee,Ae=e.showTransferBrowseEntryOnboarding,Ne=void 0!==Ae&&Ae,Le=e.expShowFileLocking,Ie=void 0!==Le&&Le,ke=(e.expDeleteUseApiV2,e.expCopyUseApiV2),Be=void 0!==ke&&ke,De=e.expMoveUseApiV2,Me=void 0!==De&&De,Ce=e.isCreateButtonMoveEnabled,Pe=void 0!==Ce&&Ce,xe=e.forceSplitShareSecondary,Ve=void 0!==xe&&xe,We=e.shouldUsePortalPopover,qe=void 0!==We&&We,ze=e.scrollableSidebarRef,He=r.first(),Qe=!n.isCurrentPathWriteable()||r.some(function(e){return e.read_only}),Ge=Qe,Ke=Qe,Je=Qe,je=Qe,Xe=r.some(function(e){return!F.File.canViewFileMembers(e,n)}),Ye=Xe,Ze=Xe,$e=r.every(F.File.isNoAccessSharedFolder),er=$e,rr=$e,nr=$e,or=r.some(F.File.isNoAccessSharedFolder),sr=!i.isMatchedMedium&&!i.isMatchedLarge,ir=X({files:r,context:n,user:s,viewType:b,sharingServiceInfo:l,extensionsFeatureFlags:d,canDisplayFolderSizes:h,isQueryingFolderSizes:I,isSearchMode:N,expFolderHistoryRollbacks:K,pinnedItems:H,expRewindUpsell:j,showAddToTransfer:Z,showAddFolderToTransfer:re,expShowFileLocking:Ie}),tr=new Array,ar=null===W?C.BrowseSuccessBannerVariants.OFF:W,lr={user:s,context:n,actionHandlers:p,files:r,firstFile:He,isSearchMode:N,source:E,viewType:b,useSearchSuccessBanner:x,expBrowseSuccessBanner:ar,isSidebar:M,expListSubfoldersWithOdyssey:z,expMoveUseApiV2:Me,expCopyUseApiV2:Be,showTransferBrowseEntryOnboarding:Ne};return!S&&ir.showTiburonShare&&tr.push(v.getShareItem(lr,{showSplitShare:ir.showSplitShare,user:s,firstFile:He,isViewSmall:sr,shareItemDisabled:Ye,isCreateButtonMoveEnabled:Pe,forceSplitShareSecondary:Ve,extensionsFeatureFlags:d,sharingServiceInfo:l,scrollableSidebarRef:ze,shouldUsePortalPopover:qe,isSidebar:M,isSearchMode:N})),ir.showCopyPublicFolderFileLink&&tr.push({label:G._("Copy public link"),iconName:"link",className:"action-world-link",performAction:ie(lr),key:"copy-public-link"}),ir.showOpen&&tr.push({label:G._("Open"),performAction:te(lr),iconName:"open-in-app",className:"action-open-in-app",key:"open"}),ir.showShowInFolder&&tr.push({label:G._("Show in folder"),performAction:ne(lr),iconName:"open-in-app",className:"action-show-in-folder",key:"show-in-folder"}),ir.showDownload&&tr.push({label:G._("Download"),performAction:ae({canShowUJPopup:B,args:lr}),disabled:er,iconName:"download",className:"action-download",key:"download"}),ir.showOpenWith&&tr.push(function(){return o.default.createElement(c.ExtensionsMenu,{user:s,file:He,triggerType:y.TriggerType.SecondaryButton,telemetryContext:{surface:"sidebar"},key:"extensions-"+He.fq_path,onDropdownOpen:oe(lr),onExtensionClick:se(lr),scrollableSidebarRef:ze,shouldUsePortalPopover:qe})}),ir.showAddToTransfer&&tr.push({label:G._("Send with Transfer"),performAction:Te(lr),iconName:"send-with-transfer"}),ir.showRequestFiles&&tr.push({label:G._("Request files"),performAction:$(lr),iconName:"new-file-request"}),ir.showComment&&tr.push({label:G._("Add comment"),performAction:le(lr),iconName:"add-comment",className:"action-add-comment",key:"add-comment"}),ir.showStar?tr.push({label:G._("Star",{comment:'Verb describing the "starring" action, not the noun.'}),ariaLabel:G._("Add to Starred"),ariaPressed:!1,performAction:ce({isStarred:!0,args:lr}),iconName:"star",className:"action-star",key:"star"}):ir.showUnstar&&tr.push({label:G._("Unstar",{comment:'Verb describing the "unstarring" action.'}),ariaLabel:G._("Add to Starred"),ariaPressed:!0,performAction:ce({isStarred:!1,args:lr}),iconName:"unstar",className:"action-unstar",key:"star"}),ir.showFileLock&&tr.push({label:G._("Lock editing"),performAction:Ue(lr),key:"file-lock",iconName:"permissions",badge:{variant:u.BadgeVariant.BETA,color:u.BadgeColor.PINK}}),ir.showFileUnlock&&tr.push({label:G._("Unlock editing"),performAction:Oe(lr),key:"file-lock",iconName:"permissions",badge:{variant:u.BadgeVariant.BETA,color:u.BadgeColor.PINK}}),ir.showFileRequestUnlock&&tr.push({label:G._("Ask to unlock"),performAction:Re(lr),key:"file-lock",iconName:"permissions",badge:{variant:u.BadgeVariant.BETA,color:u.BadgeColor.PINK}}),ir.showVersions&&(void 0===lr.firstFile.is_versionable||lr.firstFile.is_versionable)&&tr.push({label:G._("Version history"),performAction:we(lr),iconName:"view-version-history",className:"action-view-version-history",key:"version-history"}),ir.showRewindFolder&&tr.push({label:G._("Rewind",{project:"folder-history"}),performAction:pe(lr),iconName:"revert-folder",className:"action-revert-folder",key:"rewind"}),ir.showRename&&tr.push({label:G._("Rename"),performAction:de(lr),iconName:"rename",className:"action-rename",disabled:Ke,key:"rename"}),ir.showRenameTeamFolder&&tr.push({label:G._("Rename"),performAction:ue(lr),iconName:"rename",className:"action-rename",key:"rename-team-folder"}),ir.showArchive&&tr.push({label:G._("Archive"),performAction:fe(lr),iconName:"delete",className:"action-delete",key:"archive"}),ir.showMove&&tr.push(_.getMoveItem(lr,{isOverFreeQuota:T,hasExcludedNsIds:O,moveItemDisabled:Je})),ir.showCopy&&tr.push(w.getCopyItem(lr,{isOverFreeQuota:T,hasExcludedNsIds:O,copyItemDisabled:rr})),ir.showDelete&&tr.push({label:G._("Delete"),performAction:_e(lr),disabled:Ge,iconName:"delete",className:"action-delete",key:"delete"}),ir.showRestore&&tr.push({label:G._("Restore"),className:"action-restore",performAction:he({args:lr}),disabled:je,key:"restore"}),ir.showReadd&&tr.push({label:G._("Re-add shared folder"),className:"action-re-add",performAction:me(lr),disabled:je,key:"re-add-shared-folder"}),ir.showPermanentDelete&&tr.push({label:G._("Permanently delete"),iconName:"delete",className:"action-permanently-delete",performAction:ve(lr),key:"permanently-delete"}),ir.showSharedFolderEvents&&tr.push({label:G._("Events"),performAction:Se(lr),disabled:Ze,iconName:"events",className:"action-events",key:"events"}),ir.showCalculateFolderSize&&tr.push({label:G._("Calculate size"),performAction:Fe(lr),disabled:nr,iconName:"calculate",className:"action-calculate-folder-size",key:"calculate-size"}),ir.showManageAsAdmin&&tr.push({label:G._("View in admin console"),performAction:ge(lr),iconName:"settings-gear",key:"view-in-admin-console"}),ir.showPin&&tr.push({label:G._("Pin to %(path)s").format({path:t.Emstring.em_snippet(a.filename(n.currentFQPath),8,1)}),performAction:ye(lr),iconName:"pin",disabled:or,key:"pin-to"}),ir.showUnpin&&tr.push({label:G._("Unpin"),performAction:be(lr),iconName:"pin",disabled:or,key:"unpin"}),ir.showPinTo&&tr.push({label:G._("Pin to..."),performAction:Ee(lr),iconName:"pin",disabled:or,key:"pin-to-path"}),tr}Object.defineProperty(r,"__esModule",{value:!0}),o=n.__importDefault(o),a=n.__importStar(a),d=n.__importStar(d),m=n.__importStar(m),b=n.__importStar(b),L=n.__importStar(L),W=n.__importDefault(W),Q=n.__importStar(Q);var re={showTiburonShare:!1,showCopyPublicFolderFileLink:!1,showOpen:!1,showOpenWith:!1,showDownload:!1,showComment:!1,showDelete:!1,showArchive:!1,showRemove:!1,showRename:!1,showRenameTeamFolder:!1,showMove:!1,showCopy:!1,showVersions:!1,showRewindFolder:!1,showRestore:!1,showReadd:!1,showPermanentDelete:!1,showRemoveAccess:!1,showShowInFolder:!1,showStar:!1,showUnstar:!1,showLoadingStar:!1,showSharedFolderEvents:!1,showCalculateFolderSize:!1,showManageAsAdmin:!1,showRequestFiles:!1,showPin:!1,showPinTo:!1,showUnpin:!1,showSplitShare:!1,showAddToTransfer:!1,showAddFolderToTransfer:!1,showTransferBrowseEntryOnboarding:!1,showFileLock:!1,showFileUnlock:!1,showFileRequestUnlock:!1},ne=function(e){var r=e.user,n=e.files,o=e.firstFile,i=n.map(function(e){return e.fq_path}).toArray();return function(){var n=i.map(function(e){return a.filename(e)}),t=a.parent_dir(i[0]),l=s.browse_uri_for_fq_path(r,t);n.length>1?l.updateQuery({select_multi:JSON.stringify(n)}):l.updateQuery({select:n[0]}),S.logUserEventOnFile({result:o,args:e,action_type:H.WebUserActionLogEvent.SHOW_IN_FOLDER}),window.open(l.toString(),"_blank")}},oe=function(e){return function(){var r=e.isSidebar?H.ActionSurfaceLogValue.RIGHT_SIDEBAR:H.ActionSurfaceLogValue.CENTER_PANE;S.logUserEventOnFile({result:e.firstFile,args:e,action_type:H.WebUserActionLogEvent.OPEN_EXTENSIONS,action_surface:r})}},se=function(e){return function(r){var n=e.isSidebar?H.ActionSurfaceLogValue.RIGHT_SIDEBAR:H.ActionSurfaceLogValue.CENTER_PANE;S.logUserEventOnFile({result:e.firstFile,args:e,action_type:H.WebUserActionLogEvent.EXTENSIONS_SELECTED,action_surface:n,extra:{extensions_option:r||"unknown"}})}},ie=function(e){return function(){var r=e.user,n=e.context,o=e.firstFile;if(n.config&&o&&o.fq_path){var s=P.makePublicFolderLink(r.id,n.config,o.fq_path,o.is_dir);s&&P.showPublicFolderLinkModal(s)}}},te=function(e){return function(){var r=e.user,n=e.firstFile;S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.OPEN_DESKTOP}),q.UnityFeatures.open_file(n.ns_id,n.ns_path,r.id,q.UnityFeatures.standard_open_file_handler,function(){return q.UnityFeatures.standard_open_file_handler(!1)})}},ae=function(e){var r=e.canShowUJPopup,n=e.args;return function(){var e=n.user,o=n.context,s=n.files,i=n.firstFile,t=n.isSearchMode,a=n.useSearchSuccessBanner,l=n.expBrowseSuccessBanner;S.logUserEventOnFile({result:i,args:n,action_type:H.WebUserActionLogEvent.DOWNLOAD}),S.showRetrievalSuccessBanner(a,t,l,H.WebUserActionLogEvent.DOWNLOAD),f.browseActions.downloadFiles({files:s,context:o,user:e}),r&&h.maybeShowClientDownloadModal(!0)}},le=function(e){return function(){var r=e.firstFile,n=e.isSearchMode,o=e.useSearchSuccessBanner,s=e.expBrowseSuccessBanner;S.logUserEventOnFile({result:r,args:e,action_type:H.WebUserActionLogEvent.ADD_COMMENT}),Ae(o,n,s,H.WebUserActionLogEvent.ADD_COMMENT),e.actionHandlers.onShowComment(r)}},ce=function(e){var r=e.isStarred,n=e.args;return function(){var e=n.user,o=n.firstFile,s=n.isSearchMode,i=n.useSearchSuccessBanner,t=n.expBrowseSuccessBanner;S.logUserEventOnFile({result:o,args:n,action_type:r?H.WebUserActionLogEvent.STAR:H.WebUserActionLogEvent.REMOVE_STAR}),S.showRetrievalSuccessBanner(i,s,t,H.WebUserActionLogEvent.STAR),M.StarredActions.update(e.role,o.file_id,M.HOME_RESOURCE_ID_TYPE.ENCODED_FILE_OBJ_ID,r)}},de=function(e){return function(){var r=e.actionHandlers,n=e.firstFile,o=e.isSearchMode,s=e.useSearchSuccessBanner,i=e.expBrowseSuccessBanner;S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.RENAME}),S.showRetrievalSuccessBanner(s,o,i,H.WebUserActionLogEvent.RENAME),r.onShowRename(n)}},ue=function(e){return function(){var r=e.files,n=e.firstFile;S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.RENAME}),d.showContentManagerBridge(r.toArray(),"rename",z.Viewer.get_viewer().team_is_limited)}},fe=function(e){return function(){var r=e.files,n=e.firstFile;S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.ARCHIVE}),d.showContentManagerBridge(r.toArray(),"archive",z.Viewer.get_viewer().team_is_limited)}},he=function(e){var r=e.args;return function(){var e=r.actionHandlers,n=r.files,o=r.firstFile,s=r.isSearchMode,i=r.useSearchSuccessBanner,t=r.expBrowseSuccessBanner,a=n.toArray();S.logUserEventOnFile({result:o,args:r,action_type:H.WebUserActionLogEvent.RESTORE}),S.showRetrievalSuccessBanner(i,s,t,H.WebUserActionLogEvent.RESTORE),e.onRestore(a)}},pe=function(e){return function(){var r=s.browse_uri_for_fq_path(e.user,e.firstFile.fq_path);r.setQuery({rewind:"true"}),Q.replace_location(r.toString())}},me=function(e){return function(){var r=e.user,n=e.context,o=e.firstFile,s=o.fq_path.toLowerCase(),i=n.inactiveNSIDByFQPath[s];i&&P.asyncMountSharedFolder(i,r.role,x.SHARE_ACTION_ORIGIN_TYPE.BROWSE_FILE_ACTIONS,!1,null,!0)}},we=function(e){return function(){var r=e.user,n=e.firstFile,o=e.isSearchMode,i=e.useSearchSuccessBanner,t=e.expBrowseSuccessBanner;S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.VERSIONS}),Ae(i,o,t,H.WebUserActionLogEvent.VERSIONS);var a=s.getFileHistoryUrl({fqPath:n.fq_path,userId:r.id});Q.redirect(a)}},_e=function(e){return function(){var r=e.actionHandlers,n=e.firstFile,o=e.files,s=e.isSearchMode,i=e.useSearchSuccessBanner,t=e.expBrowseSuccessBanner;S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.DELETE}),S.showRetrievalSuccessBanner(i,s,t,H.WebUserActionLogEvent.DELETE),r.onDelete(o.toArray())}},ve=function(e){return function(){var r=e.actionHandlers,n=e.files,o=e.firstFile,s=e.isSearchMode,i=e.useSearchSuccessBanner,t=e.expBrowseSuccessBanner;S.logUserEventOnFile({result:o,args:e,action_type:H.WebUserActionLogEvent.PERMANENT_DELETE}),S.showRetrievalSuccessBanner(i,s,t,H.WebUserActionLogEvent.PERMANENT_DELETE),r.onPermanentDelete(n.toArray())}},Se=function(e){return function(){var r=e.firstFile;S.logUserEventOnFile({result:r,args:e,action_type:H.WebUserActionLogEvent.EVENTS}),Q.redirect("/events?ns="+r.target_ns)}},Fe=function(e){return function(){var r=e.firstFile,n=e.files;S.logUserEventOnFile({result:r,args:e,action_type:H.WebUserActionLogEvent.CALCULATE_SIZE}),f.browseActions.fetchMultipleFolderSizes({files:n})}},ge=function(e){return function(){K({containingFolder:e.context.currentFQPath})}},ye=function(e){return function(){var r=e.files,n=e.firstFile,o=I.getStoreForFolderOverview();o.dispatch(L.addContentReferences({folderFileId:m.folderOverviewFolderFileId(o.getState()),fileIds:r.toArray().map(k.fileToContentReferenceId),componentNamespace:p.BrowseFolderOverviewComponentNamespace})),S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.FOLDER_OVERVIEW_PIN})}},Ee=function(e){return function(){var r=e.files,n=e.firstFile,o=e.user,s=e.context,i=function(o){I.getStoreForFolderOverview().dispatch(L.addContentReferencesForPath({files:r.toArray(),path:o})),S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.FOLDER_OVERVIEW_PIN})};N.asyncShowPinToModal({user:o,path:s.currentFQPath,files:r.toArray(),onFolderSelected:i,isCurrentPathReadOnly:!s.isCurrentPathWriteable()})}},be=function(e){return function(){var r=e.files,n=e.firstFile,o=I.getStoreForFolderOverview();o.dispatch(L.removeContentReferences({folderFileId:m.folderOverviewFolderFileId(o.getState()),fileIds:r.toArray().map(k.fileToContentReferenceId),componentNamespace:p.BrowseFolderOverviewComponentNamespace})),S.logUserEventOnFile({result:n,args:e,action_type:H.WebUserActionLogEvent.FOLDER_OVERVIEW_UNPIN})}},Ae=function(e,r,n,o){e&&r&&B.setSearchSuccessBannerVisible(),n!==C.BrowseSuccessBannerVariants.V2||r||B.setBrowseSuccessBannerVisible(o)},Te=function(e){return function(){var r=function(){var r=e.user.id,n=e.files.map(function(e){return{file_id:e.file_id,destination_path:a.filename(e.ns_path)}}).toArray();D.Snackbar.sync(G._("Creating new transfer...",{comment:"Message shown when creating a new transfer from file list on web"}),!1,"create-transfer"),T.createTransfer(r,O.generateTransferTrackingId()).then(function(e){if(!e.transfer)return Promise.reject();var o=e.transfer.transfer_id;return T.addFilesById(r,o,n).then(function(){D.Snackbar.close("create-transfer"),Q.redirect(U.editTransferUrl(o,{referrer:"browse",logCreateTransferEvent:!0}))})}).catch(function(){D.Snackbar.fail(G._("Unable to create transfer",{comment:"Error message shown when creating a new transfer from file list on web"}),"create-transfer")})};e.showTransferBrowseEntryOnboarding?A.asyncShowBrowseEntryPointOnboardingModal({primaryActionOnClick:r,userId:e.user.id}):r()}},Ue=function(e){return function(){b.showFileLockConfirmModal(e.user.id,e.firstFile),S.logUserEventOnFile({result:e.firstFile,args:e,action_type:H.WebUserActionLogEvent.LOCK_FILE})}},Oe=function(e){return function(){b.asyncUnlockFile(e.user.id,[e.firstFile]),S.logUserEventOnFile({result:e.firstFile,args:e,action_type:H.WebUserActionLogEvent.UNLOCK_FILE})}},Re=function(e){return function(){b.showRequestUnlockConfirmModal(e.user.id,e.firstFile),S.logUserEventOnFile({result:e.firstFile,args:e,action_type:H.WebUserActionLogEvent.REQUEST_UNLOCK_FILE})}};r.getActionsForFileCollection=ee});
//# sourceMappingURL=file_actions.min.js-vfl5Qjqrj.map