<?php

include("library.php");
$result = translate_contract($_POST['contract']);

if (hasErrors($result))
    echo "n";
else
    echo "y";

?>