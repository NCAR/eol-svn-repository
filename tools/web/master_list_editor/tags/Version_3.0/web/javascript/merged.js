function hoverCategory(row, isHover) {
    if (isHover) {
        row.oldClassName = row.className;
        row.className='categoryListTableHover';
    } else {
        if (row.oldClassName != '') {
            row.className = row.oldClassName;
            row.oldClassName = '';
        }
    }
    return true;
}

/**
 * Change the CSS class for a dataset row when for onmouseover and onmouseout
 * events.
 * @param row The dataset row that is having the style changed.
 * @param isHover <code>true</code> if the dataset row is being hovered over,
 * <code>false</code> otherwise.
 **/
function hoverDataset(row, isHover) {
    if (isHover) { 
        row.oldClassName = row.className;
        row.className='datasetEntryRowHover';
    } else {
        if (row.oldClassName != '') {
            row.className=row.oldClassName;
            row.oldClassName='';
        }
    }
    return true;
}

function openHelpWindow(url) {
    window.open(url,"mlHelp","width=600,height=600,menubar=no,toolbar=no,"+
        "location=no,resizable=yes,scrollbars=yes,status=0").focus();
}

function scrollTo(menuId,expanderId,projectId,categoryId,datasetId,phaseId) {
    var mainDiv = document.getElementById("main");

    if (projectId != '') {
        var projects = document.getElementsByName("project:"+projectId);
        if (projects.length > 0) {
            var offParent = projects[0].offsetParent;
            var offset = projects[0].offsetTop;
            while (offParent != mainDiv) {
                offset = offParent.offsetTop;
                offParent = offParent.offsetParent;
            }
            mainDiv.scrollTop = offset - (mainDiv.offsetHeight / 2);
        }
    }

    if (categoryId != '') {
        var cats = document.getElementsByName("category:"+categoryId);
        var offParent = cats[0].offsetParent;
        var offset = cats[0].offsetTop;
        while (offParent != mainDiv) {
            offset = offParent.offsetTop;
            offParent = offParent.offsetParent;
        }
        mainDiv.scrollTop = offset - (mainDiv.offsetHeight/2);
    }

    if (phaseId != '') {
        var phases = document.getElementsByName("phase:"+phaseId);
        var offParent = phases[0].offsetParent;
        var offset = phases[0].offsetTop;
        while (offParent != mainDiv) {
            offset = offParent.offsetTop;
            offParent = offParent.offsetParent;
        }
        mainDiv.scrollTop = offset - (mainDiv.offsetHeight/2);
    }

    if (datasetId != '') {
        var datasets = document.getElementsByName("dataset:"+datasetId);
        var offParent = datasets[0].offsetParent;
        var offset = datasets[0].offsetTop;
        while (offParent != mainDiv) {
            offset = offParent.offsetTop;
            offParent = offParent.offsetParent;
        }
        mainDiv.scrollTop = offset - (mainDiv.offsetHeight / 2);
    }
}