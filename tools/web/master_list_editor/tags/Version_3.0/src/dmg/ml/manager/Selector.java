package dmg.ml.manager;

import dmg.jsf.model.TreeNode;
import dmg.ml.MasterListException;
import dmg.ml.bean.ClassificationBean;
import dmg.ml.bean.ProjectClassificationTreeRootBean;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import javax.faces.event.ActionEvent;
import javax.faces.model.SelectItem;
import javax.faces.model.SelectItemGroup;

/**
 * <p>The Selector class is the backing component for two lists, and four 
 * buttons in a JSF page.  It contains a list of available entries that can be
 * either included or excluded.  It provides the functionality to exclude all
 * the entries in the included list, exclude the selected items in the included
 * list, include all of the items in the excluded list, and include the selected
 * items in the excluded list.</p>
 *
 * @author Joel Clawson
 */
public class Selector {
    
    private HashMap<Integer,SelectorEntry> inclusionMap;
    private String[] selectedExcluded, selectedIncluded;
    private List<String> types;
    
    /**
     * Create a new instance of a Selector.
     * @param data The complete list of available classifications that can be 
     * selected.
     **/
    public Selector(List<? extends Selectable> data, List<String> types) {
        inclusionMap = new HashMap<Integer,SelectorEntry>();
        for (Selectable item: data) {
            inclusionMap.put(item.getSelectableId(),new SelectorEntry(item,Boolean.FALSE));
        }

	this.types = types;
    }
    
    /**
     * Exclude all of the entries that are currently in the include list.
     * @param evt The event that triggered the exclusion.
     **/
    public void excludeAll(ActionEvent evt) {
        for (SelectorEntry entry: inclusionMap.values()) {
            entry.setIncluded(Boolean.FALSE);
        }
    }
    
    /**
     * Exclude all of the entries that are currently selected in the included
     * list. 
     * @param evt The event that triggered the item list.
     **/
    public void excludeSelected(ActionEvent evt) {
        for (int i = 0; i < getSelectedIncluded().length; i++) {
            Integer id = Integer.valueOf(getSelectedIncluded()[i]);
            inclusionMap.get(id).setIncluded(Boolean.FALSE);            
        }
    }

    /**
     * Generate a list of SelectItem entries that can be used in a HTML form
     * in a JSF page.
     * @param included <code>true</code> if the list should be of the included
     * entries, <code>false</code> if the list is for excluded entries.
     * @return The list of entries to be displayed.
     **/
    private List<SelectItem> generateList(boolean included)  {
        List<SelectItem> list = new ArrayList<SelectItem>();

	HashMap<String,List<SelectItem>> typeMap = new HashMap<String,List<SelectItem>>();
	for (Iterator<String> itr = types.iterator(); itr.hasNext(); ) {
	    typeMap.put(itr.next(),new ArrayList<SelectItem>());
	}

        // A state holder for option groups (classifications with subclassifications) to
        // add all of its children to the group.
        HashMap<Integer,List<SelectItem>> groupMap = new HashMap<Integer,List<SelectItem>>();

        for (SelectorEntry entry: inclusionMap.values()) {
            
            // Only care if the entry is to be in the list.
            if (entry.isIncluded() == included) {
                Selectable selected = entry.getEntry();
                
		try {
                    // Doesn't have a parent, so it won't be in an option group.
                    if (!selected.hasParent()) {
			typeMap.get(selected.getType()).add(new SelectItem(selected.getSelectableId().toString(),selected.getName()));
                    } else {
                        for (Selectable parent: selected.getParents()) {
                            
                            // Determine if the option group has already been created.
                            if (groupMap.get(parent.getSelectableId()) == null) {
                                groupMap.put(parent.getSelectableId(),new ArrayList<SelectItem>());
                            }
                            // Add the entry to the entry group.
                            groupMap.get(parent.getSelectableId()).add(new SelectItem(selected.getSelectableId().toString(),parent.getName()+":"+selected.getName()));
                        }
                    }
                } catch (MasterListException e) {}
            }
        }
        
        for (Integer entryId: groupMap.keySet()) {
            // Need to check for null, so items that have been removed will not
            // display their children.
            
            if (inclusionMap.get(entryId) != null) {
                typeMap.get(inclusionMap.get(entryId).getEntry().getType()).addAll(groupMap.get(entryId));
            }
        }

        for (String type: typeMap.keySet()) {
	    SelectItemGroup typeGroup = new SelectItemGroup(type);	    
	    SelectItem[] itemList = new SelectItem[typeMap.get(type).size()];
	    if (itemList.length > 0) {
                typeMap.get(type).toArray(itemList);
                Arrays.sort(itemList,new SelectItemComparator());
                typeGroup.setSelectItems(itemList); 
                list.add(typeGroup);
            }
	}

        // Now sort the classifications so they are in alphabetical order.
	Collections.sort(list,new SelectItemComparator());
        return list;
    }

    /**
     * Get the list of entries that are to be displayed in the excluded list.
     * @return The list of items in the selector that are currently excluded.
     **/
    public List<SelectItem> getExcluded() { return generateList(false); }
    
    /**
     * Get the list of entries that are to be displayed in the included list.
     * @return The list of items in the selector that are currently included.
     **/
    public List<SelectItem> getIncluded() { return generateList(true); }
    
    /**
     * Get the list of classifications in the Selector that are marked as included.
     * @return The list of included classifications in the Selector.
     **/
    public List<Selectable> getIncludedItems() {
        List<Selectable> list = new ArrayList<Selectable>();
        for (SelectorEntry entry: inclusionMap.values()) {
            if (entry.isIncluded()) { list.add(entry.getEntry()); }
        }
        return list;
    }
    
    /**
     * Get the list of Strings that are currently selected in the excluded list.
     * @return The Strings that are selected in the excluded list.
     **/
    public String[] getSelectedExcluded() { return selectedExcluded; }

    /**
     * Get the list of Strings that are currently selected in the included list.
     * @return The Strings that are selected in the included list.
     **/
    public String[] getSelectedIncluded() { return selectedIncluded; }
    
    /**
     * Include all of the entries that are currently in the excluded list.
     * @param evt The event that triggered the inclusion.
     **/
    public void includeAll(ActionEvent evt) {
        for (SelectorEntry entry: inclusionMap.values()) {
            entry.setIncluded(Boolean.TRUE);
        }
    }

    /**
     * Include the entries that are selected in the excluded list.
     * @param evt The event that triggered the inclusion.
     **/
    public void includeSelected(ActionEvent evt) {
        for (int i = 0; i < getSelectedExcluded().length; i++) {
            Integer id = Integer.valueOf(getSelectedExcluded()[i]);
            SelectorEntry entry = inclusionMap.get(id);
            entry.setIncluded(Boolean.TRUE);
        }
    }

    /**
     * Set the list of classification as included in the Selector.
     * @param data The list to mark as included in the Selector.
     **/
    public void setIncluded(List<? extends Selectable> data) {
        for (Selectable item: data) {
            SelectorEntry entry = inclusionMap.get(item.getSelectableId());
            if (entry != null) { entry.setIncluded(Boolean.TRUE); }
        }
    }
    
    /**
     * Set the selected items in the excluded list.
     * @param items The entries in the excluded list that are selected.
     **/
    public void setSelectedExcluded(String[] items) {
        selectedExcluded = items;
    }
    
    /**
     * Set the selected items in the included list.
     * @param items The entries in the included list that are selected.
     **/
    public void setSelectedIncluded(String[] items) {
        selectedIncluded = items;
    }
    
    /**
     * <p>The ClassificationSelectItemComparator is a Comparator for sorting
     * ClassificationBeans wrapped in a SelectItem by their name.</p>
     **/
    private class SelectItemComparator implements Comparator<SelectItem> {
 
        /**
         * Determine the sort order or two items.
         * @param item1 The first item to be compared.
         * @param item2 The second item to be compared.
         * @return A negative number if item1 < item2, 0 if item1 == item2, or
         * a positive number if item1 > item2.
         **/
        public int compare(SelectItem item1, SelectItem item2) {
	    if (!item1.getLabel().equals(item2.getLabel())) {
		return item1.getLabel().compareTo(item2.getLabel());
	    } else if (item1 instanceof SelectItemGroup) {
		return 1;
	    } else {
		return -1;
	    }
	}
    }
    
    /**
     * <p>The SelectorEntry class is a data holder for a Selector that pairs
     * an inclusion status with a ClassificationBean.</p>
     **/
    private class SelectorEntry {
        
        private Selectable entry;
        private Boolean included;
        
        /**
         * Create a new instance of a SelectorEntry.
         * @param classification The classification being contained in the entry.
         * @param included <code>true</code> if the classification is to be in the
         * included list, <code>false</code> if the classification is to be in the
         * excluded list.
         **/
        public SelectorEntry(Selectable entry, Boolean included) {
            this.entry = entry;
            this.included = included;
        }
        
        /**
         * Get the classification that is held in this entry.
         * @return The classification in the entry.
         **/
        public Selectable getEntry() { return entry; }
        
        /**
         * Determine the inclusion status of the classification in the entry.
         * @return <code>true</code> if the classification is marked as included,
         * <code>false</code> if it is marked as excluded.
         **/
        public Boolean isIncluded() { return included; }
        
        /**
         * Set the inclusion status of the classification contained in this entry.
         * @param <code>true</code> if the classification is to be in the included 
         * list, <code>false</code> if it is to be in the excluded list.
         **/
        public void setIncluded(Boolean included) { this.included = included; }
    }
}
