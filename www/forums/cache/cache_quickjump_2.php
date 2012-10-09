<?php

if (!defined('PUN')) exit;
define('PUN_QJ_LOADED', 1);

?>				<form id="qjump" method="get" action="viewforum.php">
					<div><label><?php echo $lang_common['Jump to'] ?>

					<br /><select name="id" onchange="window.location=('viewforum.php?id='+this.options[this.selectedIndex].value)">
						<optgroup label="Official">
							<option value="3"<?php echo ($forum_id == 3) ? ' selected="selected"' : '' ?>>Annoucements</option>
						</optgroup>
						<optgroup label="Support">
							<option value="4"<?php echo ($forum_id == 4) ? ' selected="selected"' : '' ?>>Opinions</option>
							<option value="2"<?php echo ($forum_id == 2) ? ' selected="selected"' : '' ?>>Known bugs</option>
							<option value="5"<?php echo ($forum_id == 5) ? ' selected="selected"' : '' ?>>Bugs</option>
							<option value="6"<?php echo ($forum_id == 6) ? ' selected="selected"' : '' ?>>Questions</option>
					</optgroup>
					</select>
					<input type="submit" value="<?php echo $lang_common['Go'] ?>" accesskey="g" />
					</label></div>
				</form>
