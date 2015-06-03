<?php 
include("co2api.lib.php");
?>

<html><head>
<title>TST Dualizer (beta)</title>
<style>
.grey {

    width:600px;
    line-height:30px;
    background:#dedede;
    padding-left:10px;
}

.white {
    width:600px;
    line-height:30px;
    padding-left:10px;
}

#footer {
    margin-top:50px;
    font-size:8pt;
    color: #999;
}
</style>
</head>
<body style="font-size:10pt; font-family:Arial; margin-left:30px">
<h1>TST Dualizer</h1>
<form method="post" action="index.php">
<div style="font-size:8pt">Type your contract here:</div>
<input type="text" name="contract" size="60" />
<input type="submit" value="Get dual" />
</form>
<?php

$con = $_POST['contract'];

if ($con != "")
{
    $trans = translate_contract($con);
    
    if (startsWith($trans, "<con"))
    {
        echo "<br/><br/><div class=\"grey\">SUBMITTED CONTRACT: <b>".htmlspecialchars($_POST['contract'])."</b></div>";
    
        echo "<div class=\"white\">ADMITS COMPLIANT: <b>".admitsCompliant($trans)."</b></div>";
    
        echo "<div class=\"grey\">KIND: <b>".kindOf($trans)."</b></div>";
    
        echo "<div class=\"white\">DUAL: <b>".dualOf($trans)."</b></div>";
    }
    else
        echo "Error: contract submitted is not valid.";
}
?>
<div id="footer">&copy; 2015 Trustworthy Computational Societies | This is a <i>beta</i> version, all right reserved.</div>
</body>
</html>