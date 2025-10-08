

<html>
<head>
   <title>DTS Development Work Flow Ideas</title>
</head>
<body>
   <h1 align="center">DTS Development Work Flow Ideas</h1>
   <p>This is a list of ideas for the flow of work on different routes to the final DTS interface.</p>

   <h4>Notes on the Times</h4>
   <p>The times for each idea are just rough estimates.</p>
   <p>The time to develop individual tools will vary on each tool.  IVEN will take longer on its initial upgrade than the other tools.  The Master List editor will probably be shorter than the other tools as it has already gone through two upgrades.</p>
  
   <hr />
 
   <h2>Idea #1 - All Out Replacement</h2>
   <p>This idea is the full replacement of all of the DTS tools with the new DTS tool at one time.</p>
   <table border="1" width="95%">
      <tr><th>Pros</th><th>Cons</th></tr>
      <tr>
         <td>
            <ul>
               <li>One step and finished.</li>
            </ul>
         </td>
         <td>
            <ul>
               <li>Very slow. (6 months - 1 year)</li>
               <li>Dependent on external factors (i.e. category/platform resolution)</li>
               <li>Could make adding new functionality/tools more difficult (publications)</li>
            </ul>
         </td>
      </tr>
   </table>
   <p>Steps required to completion:</p>
   <ol>
      <li>Design the new DTS database and hook it with CODIAC.</li>
      <li>Reconcile the DLN, IVEN, ML, and JEDI databases with the same information.</li>
      <li>Port the DLN, IVEN, and ML databases into JEDI and the new DTS database (test version).</li>
      <li>Design and develop the new DTS interface.</li>
      <li>Test the new DTS interface.</li>
      <li>Reconcile the DLN, IVEN, ML, and JEDI databases (not allowing any changes to DLN, IVEN, and ML).</li>
      <li>Port the DLN, IVEN, and ML databases into JEDI and the new DTS database (production version).</li>
      <li>Deploy the production version for the DTS interface.</li>
   </ol>

   <hr />

   <h2>Idea #2 - Individual Tool Clean Up</h2>
   <p>This idea is what has been already done with the Master List.  The underlying databases would be cleaned up and replaces with better databases, but would still be independent of the other databases.  The database would be designed to make it as simple as possible to merge with the other databases at a later time.  The tools would be upgraded to handle the new database and cleaned up so pieces could be reused for the final tool.</p>
   <table border="1" width="95%">
      <tr><th>Pros</th><th>Cons</th></tr>
      <tr>
         <td>
            <ul>
               <li>Fairly quick.  Progress would be more obvious. (1 - 2 months per tool)</li>
               <li>Individual tools could have new functionality added and wouldn't have to wait until the final product.</li>
               <li>Easier to add new tools before the completion of the final product.</li>
               <li>May learn better ways of dealing with requirements to use in the final product.</li>
               <li>Easiest to make progress without knowing the full plans for EMDAC.</li>
            </ul>
         </td>
         <td>
            <ul>
               <li>More work.  There will be multiple upgrades to each tool.</li>
               <li>More time.  It will take longer to get to the final product (or even to the use of shared databases).</li>
            </ul>
         </td>
      </tr>
   </table>
   <p>Steps required for completion:</p>
   <ol>
      <li>Individual Tool Upgrades (for each tool [DLN, IVEN])
         <ol>
            <li>Design the new tool database.</li>
            <li>Reconcile the original database with itself (if necessary)</li>
            <li>Port the database to the new one (test version)</li>
            <li>Upgrade the current tool interface.</li>
            <li>Test the new tool</li>
            <li>Finalize self reconciliation on the original database.</li>
            <li>Port the database to the new one (production version).</li>
            <li>Deploy the production version of the tool.</li>
         </ol>
      </li>
      <li>Design the new DTS database and hook it with CODIAC.</li>
      <li>Reconcile the DLN, IVEN, ML, and JEDI databases with the same information.</li>
      <li>Port the DLN, IVEN, and ML databases into JEDI and the new DTS database (test version).</li>
      <li>Design and develop the new DTS interface.</li>
      <li>Test the new DTS interface.</li>
      <li>Reconcile the DLN, IVEN, ML, and JEDI databases (not allowing any changes to DLN, IVEN, and ML).</li>
      <li>Port the DLN, IVEN, and ML databases into JEDI and the new DTS database (production version).</li>
      <li>Deploy the production version for the DTS interface.</li>
   </ol>

   <hr />

   <h3>Idea #3 - Individual Tool Clean Up on Merged Databases</h3>
   <p>This is the idea to merge the databases into the DTS databases and upgrade the existing tools to use them before developing the merged interface.</p>
   <table border="1" width="95%">
      <tr><th>Pros</th><th>Cons</th></tr>
      <tr>
         <td>
            <ul>
               <li>Probably not as much work as idea #2 (but still more than #1)</li>
               <li>More noticeable progress than idea #1 (at best the same speed as #2)</li>
               <li>Upgrades to the tools will be using the same databases, thus the same information.</li>
            </ul>
         </td>
         <td>
            <ul>
               <li>More work than idea #1 (but probably less than #2) (2-4 weeks on the database; 1-2 months per tool)</li>
               <li>More complex than idea #2 to merge databases (same as #1)</li>
               <li>Not obvious that the information between tools is shared.</li>
               <li>May need to deploy all tool upgrades at the same time to prevent confusion.</li>
            </ul>
         </td>
      </tr>
   </table>
   <p>Steps required for completion:</p>
   <ol>
      <li>Design the new DTS database and hook it with CODIAC.</li>
      <li>Reconcile the DLN, IVEN, ML, and JEDI databases with the same information.</li>
      <li>Port the DLN, IVEN, and ML databases into JEDI and the new DTS database (test version).</li>
      <li>Upgrade all of the tools to use the new database.</li>
      <li>Test the new tools.</li>
      <li>Reconcile the DLN, IVEN, ML, and JEDI databases (not allowing any changes to DLN, IVEN, and ML).</li>
      <li>Port the DLN, IVEN, and ML databases into JEDI and the new DTS database (production version).</li>
      <li>Deploy the upgraded tools</li>
      <li>Design and develop the new DTS interface.</li>
      <li>Test the new DTS interface.</li>
      <li>Deploy the production version of the DTS interface.</li>
   </ol>

</body>
</html>
