# Tyranid
Tyranid is a free, open-source framework designed to help process and collect information using strong run-time typing using the Scala language.

The library is currently in its early stages and the API will likely change frequently until version 1.0 is reached.

More information can be found at:

http://tyranid.org

Threading
---------

<table>
<tr><th>Scope</th><th>Threadsafe</th><th>Classes</th></tr>
<tr><td>Global</td><td>Yes</td><td>Entity/Attribute</td></tr>
<tr><td>Global</td><td>Yes</td><td>View/ViewAttribute</td></tr>
<tr><td>Global</td><td>Yes</td><td>Ui/Grid/Field/etc.</td></tr>
<tr><td>Session</td><td>No</td><td>Record</td></tr>
<tr><td>Request</td><td>No</td><td>Scope</td></tr>
</table>


