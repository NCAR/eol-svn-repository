function addDataset(productId) {
   windowOpener('edit_dataset?product='+productId);
}

function addGroupDataset(parentId) {
  windowOpener('add_group_association?id='+parentId);
}

function addProduct(projectId) {
   windowOpener('add_product?project='+projectId);
}

function addSourceDataset(parentId) {
   windowOpener('add_dataset_association?dataset='+parentId);
}

function changeProductType(typeSelect) {
   var type = typeSelect.options[typeSelect.selectedIndex].value;
   var trs = document.getElementsByTagName("tr");

   for (var i = 0; i < trs.length; i++) {
      if (trs[i].id == '') { /* Don't do anything */ }
      else if (trs[i].id.indexOf(type) == 0) { trs[i].style.display = 'table-row'; }
      else { trs[i].style.display = 'none'; }
   }
}

function closeSuccess() {
   window.opener.location.reload();
   closeWindow();
}

function closeWindow() { window.close(); }

function confirmDelete(button) {
   return confirm('Are you sure you want to delete the entry?\n'+
                  'This will delete all notes, associations with other datasets, etc.\n');
}

function editDataset(datasetId) {
   windowOpener('edit_dataset?dataset='+datasetId);
}

function editProduct(url) {
   windowOpener(url);
}

function editSource(datasetId) {
   windowOpener('edit_source_info?dataset='+datasetId);
}

function editSourceAfterLink(datasetId) {
   window.opener.location.reload();
   window.location.href = 'edit_source_info?dataset='+datasetId;
}

function excludeDataset(datasetId,sourceId) {
   windowOpener('exclude_dataset_action?dataset='+datasetId+'&source='+sourceId+'&mode=exclude');
}

function excludeGroup(groupId,sourceId) {
   windowOpener('exclude_group_action?group='+groupId+'&source='+sourceId+'&mode=exclude');
}

function includeDataset(datasetId,sourceId) {
   windowOpener('exclude_dataset_action?dataset='+datasetId+'&source='+sourceId+'&mode=include');
}

function includeGroup(groupId,sourceId) {
   windowOpener('exclude_group_action?group='+groupId+'&source='+sourceId+'&mode=include');
}

function noteExpander(divId,divClass) {
   var noteElem = document.getElementById(divId);
   var noteImg = document.getElementById(divId+"Img");

   if (noteElem.className == divClass) {
      noteElem.className = divClass+"Expanded";
      noteImg.src = noteImg.src.replace(/e_notes.gif/,"s_notes.gif");
   } else {
      noteElem.className = divClass;
      noteImg.src = noteImg.src.replace(/s_notes.gif/,"e_notes.gif");
   }
}

function projectNoteEditor(projectId,noteId) {
   windowOpener('edit_note?project='+projectId+'&id='+noteId);
}

function selectProduct(projectId,dropdown) {
   var product = dropdown.options[dropdown.selectedIndex].value;
   var pieces = product.match(/(grp|cmp)([\d\.]+)/);

   if (pieces[1] == "grp") {
      window.location.href = "group_view?project="+projectId+"&id="+pieces[2];
   } else {
      window.location.href = "dataset_view?project="+projectId+"&id="+pieces[2];
   }
}

function validateDataset(form) {
   if (form.dataset.value == '') {
      alert("Dataset id cannot be empty.!\n");
      return false;
   }
   return true;
}

function validateDatasetSourceAssociation(form) {
   if (form.source.value == '') {
      alert('The source dataset id cannot be empty!\n');
      return false;
   }
   return true;
}

function validateDatasetSourceDelete(datasetId,sourceId) {
   if (confirm('Continue with removing dataset '+sourceId+' as a source for dataset '+datasetId+'?')) {
      windowOpener('remove_dataset_association_action?dataset='+datasetId+'&source='+sourceId);
   }
}

function validateGroup(form) {
   if (form.group_name.value == '') {
      alert('The Group Name field must not be empty.');
      return false;
   }
   return true;
}

function validateGroupSourceDelete(groupId,sourceId) {
   if (confirm('Continue with removing dataset '+sourceId+' as a source for group '+groupId+'?')) {
      windowOpener('remove_group_association_action?group='+groupId+'&source='+sourceId);
   }
}

function validateProduct(form) {
   if (form.type.value == 'Composite') {
      if (form.dataset_id.value == '') {
	 alert("Dataset Id for a Composite cannot be empty!\n");
  	 return false;
      } else if (form.dataset_name.value == '') {
         alert("Dataset Name for a Composite cannot be empty!\n");
         return false;
      }
      return confirm("This will create a new dataset in both JEDI and the DTS.  Continue?");
   } else if (form.type.value == 'Group') {
      if (form.group_name.value == '') {
         alert("Group Name for a Group cannot be empty!\n");
         return false;
      }
   } else {
      alert("Unknown type of product: "+form.type.value);
      return false;
   }
   return true;
}

function windowOpener(url) {
   window.open(url,"ivenWin","width=680,height=680,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0").focus();
}

