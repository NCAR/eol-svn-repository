package dmg.ml.manager;

import dmg.jsf.model.TreeNode;
import dmg.ml.bean.general.CategoryBean;
import dmg.ml.bean.general.ProjectCategoryTreeRootBean;
import java.util.ArrayList;
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
    
    /**
     * Create a new instance of a Selector.
     * @param data The complete list of available categories that can be 
     * selected.
     **/
    public Selector(List<? extends CategoryBean> data) {
        inclusionMap = new HashMap<Integer,SelectorEntry>();
        for (Iterator<?> itr = data.iterator(); itr.hasNext(); ) {
            CategoryBean category = (CategoryBean)itr.next();
            inclusionMap.put(category.getCategoryId(),
                    new SelectorEntry(category,Boolean.FALSE));
        }
    }
    
    /**
     * Exclude all of the entries that are currently in the include list.
     * @param evt The event that triggered the exclusion.
     **/
    public void excludeAll(ActionEvent evt) {
        for (Iterator<SelectorEntry> itr = 
                inclusionMap.values().iterator(); itr.hasNext(); ) {
            itr.next().setIncluded(Boolean.FALSE);
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

        // Loop through all of the entries and include all parent categories
        // that have one of their children included.
        /*
         * This is currently commented out because data sets should not have
         * all of the parents assigned to true because this messes up the
         * data set list display.
        for (Iterator<SelectorEntry> itr = 
                inclusionMap.values().iterator(); itr.hasNext(); ) {
            SelectorEntry entry = itr.next();
            if (entry.getCategory().getParentCategoryId() != null && 
                    entry.isIncluded()) {
                inclusionMap.get(entry.getCategory().getParentCategoryId()).
                        setIncluded(Boolean.TRUE);
            }
        }
        */
    }

    /**
     * Generate a list of SelectItem entries that can be used in a HTML form
     * in a JSF page.
     * @param included <code>true</code> if the list should be of the included
     * entries, <code>false</code> if the list is for excluded entries.
     * @return The list of entries to be displayed.
     **/
    private List<SelectItem> generateList(boolean included) {
        List<SelectItem> list = new ArrayList<SelectItem>();

        // A state holder for option groups (categories with subcategories) to
        // add all of its children to the group.
        HashMap<Integer,List<SelectItem>> groupMap = 
                new HashMap<Integer,List<SelectItem>>();

        for (Iterator<SelectorEntry> itr = 
                inclusionMap.values().iterator(); itr.hasNext(); ) {
            SelectorEntry entry = itr.next();
            
            // Only care if the entry is to be in the list.
            if (entry.isIncluded() == included) {
                CategoryBean category = entry.getCategory();
                // Doesn't have a parent, so it won't be in an option group.
                if (category.getParentCategoryId() == null) {
                    list.add(new SelectItem(category.getCategoryId().toString(),
                            category.getName()));
                } else {
                    // Determine if the option group has already been created.
                    if (groupMap.get(category.getParentCategoryId()) == null) {
                        groupMap.put(category.getParentCategoryId(),
                                new ArrayList<SelectItem>());
                    }
                    // Add the entry to the entry group.
                    groupMap.get(category.getParentCategoryId()).
                            add(new SelectItem(category.getCategoryId().
                                            toString(),category.getName()));
                }
            }
        }
        
        // Add all of the option groups in the map to the list.
        for (Iterator<Integer> itr = 
                groupMap.keySet().iterator(); itr.hasNext(); ) {
            Integer categoryId = itr.next();
            SelectItemGroup itemGroup = 
                    new SelectItemGroup(inclusionMap.get(categoryId).
                                                    getCategory().getName());
            SelectItem[] itemList = 
                    new SelectItem[groupMap.get(categoryId).size()];
            groupMap.get(categoryId).toArray(itemList);
            itemGroup.setSelectItems(itemList);
            list.add(itemGroup);
        }

        // Now sort the categories so they are in alphabetical order.
        Collections.sort(list,new CategorySelectItemComparator());
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
     * Get the list of categories in the Selector that are marked as included.
     * @return The list of included categories in the Selector.
     **/
    public List<CategoryBean> getIncludedCategories() {
        List<CategoryBean> list = new ArrayList<CategoryBean>();
        for (Iterator<SelectorEntry> itr = 
                inclusionMap.values().iterator(); itr.hasNext(); ) {
            SelectorEntry entry = itr.next();
            if (entry.isIncluded()) { list.add(entry.getCategory()); }
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
        for (Iterator<SelectorEntry> itr = 
                inclusionMap.values().iterator(); itr.hasNext(); ) {
            itr.next().setIncluded(Boolean.TRUE);
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

            // Mark the parent for inclusion as well
            /*
             * This is currently commented out because data sets should not have
             * all of the parents assigned to true because this messes up the
             * data set list display.
            if (entry.getCategory().getParentCategoryId() != null) {
                inclusionMap.get(entry.getCategory().getParentCategoryId()).
                        setIncluded(Boolean.TRUE);
            }
             */
        }
    }

    /**
     * Set the list of categories as included in the Selector.
     * @param categories The list to mark as included in the Selector.
     **/
    public void setIncluded(List<? extends CategoryBean> categories) {
        for (Iterator<?> itr = 
                categories.iterator(); itr.hasNext(); ) {
            CategoryBean category = (CategoryBean)itr.next();
            SelectorEntry entry = inclusionMap.get(category.getCategoryId());
            if (entry != null) {
                entry.setIncluded(Boolean.TRUE);
            }

            /*
             * This is currently commented out because data sets should not have
             * all of the parents assigned to true because this messes up the
             * data set list display.
            if (category.getParentCategoryId() != null) {
                CategoryBean parent = 
                        inclusionMap.get(category.getParentCategoryId()).
                                                                getCategory();
                inclusionMap.put(parent.getCategoryId(),
                                    new SelectorEntry(parent,Boolean.TRUE));
            }
             */
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
     * <p>The CategorySelectItemComparator is a Comparator for sorting
     * CategoryBeans wrapped in a SelectItem by their name.</p>
     **/
    private class CategorySelectItemComparator 
            implements Comparator<SelectItem> {
 
        /**
         * Determine the sort order or two items.
         * @param item1 The first item to be compared.
         * @param item2 The second item to be compared.
         * @return A negative number if item1 < item2, 0 if item1 == item2, or
         * a positive number if item1 > item2.
         **/
        public int compare(SelectItem item1, SelectItem item2) {
            if (item1.getLabel().equals(item2.getLabel())) {
                if (item1 instanceof SelectItemGroup) {
                    return 1;
                } else {
                    return -1;
                }
            } else {
                return item1.getLabel().compareTo(item2.getLabel());
            }
        }
    }
    
    /**
     * <p>The SelectorEntry class is a data holder for a Selector that pairs
     * an inclusion status with a CategoryBean.</p>
     **/
    private class SelectorEntry {
        
        private CategoryBean category;
        private Boolean included;
        
        /**
         * Create a new instance of a SelectorEntry.
         * @param category The category being contained in the entry.
         * @param included <code>true</code> if the category is to be in the
         * included list, <code>false</code> if the category is to be in the
         * excluded list.
         **/
        public SelectorEntry(CategoryBean category, Boolean included) {
            this.category = category;
            this.included = included;
        }
        
        /**
         * Get the category that is held in this entry.
         * @return The category in the entry.
         **/
        public CategoryBean getCategory() { return category; }
        
        /**
         * Determine the inclusion status of the category in the entry.
         * @return <code>true</code> if the category is marked as included,
         * <code>false</code> if it is marked as excluded.
         **/
        public Boolean isIncluded() { return included; }
        
        /**
         * Set the inclusion status of the category contained in this entry.
         * @param <code>true</code> if the category is to be in the included 
         * list, <code>false</code> if it is to be in the excluded list.
         **/
        public void setIncluded(Boolean included) { this.included = included; }
    }
}
