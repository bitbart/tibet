<?php

include("library.php");

$flag = $_POST['action'];

if ($flag == "dual") {

    $con = $_POST['contract'];
    if ($con != "")
    {
        $trans = translate_contract($con);
        
        if (startsWith($trans, "<con"))
        {
            $dual_c = htmlspecialchars($_POST['contract']);
            $dual_admit_compl = admitsCompliant($trans);
            $dual_kind = kindOf($trans);
            $dual_c_dual = dualOf($trans);
        }
        else
            $dual_err = "Error: contract submitted is not valid.";
    }
}
else if ($flag == "compliance") {
    // Controllo se l'input C1 viola i simboli permessi
    if (isset($_POST['c1s']) && preg_match("/^[a-zA-Z0-9\?\'\[\]\<\>\!\s\{\}\,\;\.\&\+\)\(]*$/", $_POST['c1s']) == 0)
    {
        $inv_input = 1; // ERRORE: C1 CONTIENE SIMBOLI VIETATI
        preg_match("/(?![a-zA-Z0-9\?\'\[\]\<\>\!\s\{\}\,\;\.\&\+\)\(]+)/", $_POST['c1s'], $matches, PREG_OFFSET_CAPTURE);
        $pos = $matches[0][1];
    }

    // Controllo se l'input C2 viola i simboli permessi
    if (isset($_POST['c2s']) && preg_match("/^[a-zA-Z0-9\?\'\[\]\<\>\!\s\{\}\,\;\.\&\+\)\(]*$/", $_POST['c2s']) == 0 && !isset($inv_input))
    {
        $inv_input = 2; // ERRORE: C2 CONTIENE SIMBOLI VIETATI
        preg_match("/(?![a-zA-Z0-9\?\'\[\]\<\>\!\s\{\}\,\;\.\&\+\)\(]+)/", $_POST['c2s'], $matches, PREG_OFFSET_CAPTURE);
        $pos = $matches[0][1];
    }

    // Controllo se c'è almeno un contratto vuoto, in tal caso restituisco un errore 
    if (isset($_POST['c1s']) && $_POST['c1s'] == "")
    {
        $inv_input = 7; // ERRORE: C1S vuoto
    }
    else if (isset($_POST['c2s']) && $_POST['c2s'] == "")
    {
        $inv_input = 8; // ERRORE: C2S vuoto
    }

    if (!isset($inv_input)) {

        if (isset($_POST['compliance']))
        {
            $translate1 = translate_contract($_POST['c1s']);
            
            if (hasErrors($translate1))
            {
                $inv_input = 5;
            }
        
            if (!isset($inv_input))
            {
                // Traduco il secondo contratto
                $translate2 = translate_contract($_POST['c2s']);
                
                if (hasErrors($translate2))
                {
                    $inv_input = 6;
                }
                
                if (!isset($inv_input))
                {
                    $response = areCompliant($translate1, $translate2);
                }
            }
        }
    }
}

?>

<html>
<head>
<title>Online TST Validator</title>
<link rel="stylesheet" type="text/css" href="style.css">
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
<script src="interface.js"></script>
</head>

<body onload="checkSyntax1();checkSyntax2()">
<div id="wrapper">
    <!--<div style="font-family:Arial; font-size:8pt; margin-bottom:15px ">
        <a href="index.php"><span style="color: #0066CC; margin-right:10px" >Home</span></a> |
        <a href="../syntax_tutorial.pdf"><span style="color: #0066CC;margin-right:10px;margin-left:10px" >Brief tutorial (PDF)</span></a> |
        <a href="/downloads/co2api"><span style="color: #0066CC; margin-right:10px;margin-left:10px" >Download CO2 Libraries (JAR)</span></a> |
        <a href="../doc/index.html"><span style="color: #0066CC; margin-right:10px;margin-left:10px">Libraries documentation</span></a>
    </div>-->

<img src="online-tst-validator.png" style="margin-bottom:15px; margin-top:-5px" />
<div style="font-family:Arial; font-size:13pt; border:1px solid #111;background: #5696BC; border-radius:10px; width:100%;">
    <div style="padding:15px">

        <h2>Compliance Checker</h2>
        
        <form method="post" action="index.php" style="margin-bottom:0px; padding-bottom:0px; font-size:8pt; color:#fff;text-shadow:1px 1px 3px #111">

        <input type="hidden" name="action" value="compliance">

        <table style="width:100%; font-size:8pt; padding:10px; color:#fff; text-shadow:1px 1px 3px #111">
        <tr><td style="width:49%;">
        Contract #1:<br />
        <textarea id="c1s" name="c1s" onkeyup="checkSyntax1();" onchange="checkSyntax1();clearSelect();fadeMyErrors();">
        <?php

        if (isset($_POST['c1s']) && !isset($_POST['clearfirst']))
        {
            echo htmlspecialchars($_POST['c1s']);
        }

        ?>
        </textarea><br>
        <div style="height:16px; margin-top:4px;float:left">
            <span style="display:inline-block;height:100%; margin-right:2px">Syntax checking: </span><span id="check1"></span>
        </div>

        <input type="submit" style="float:right; width:120px;margin-top:3px; border:1px solid #111; border-radius:3px; font-size:8pt; line-height:20px"  value="CLEAR" name="clearfirst" />
        </td><td style="width:2%"></td><td style="width:49%">
        Contract #2:<br />
        <textarea id="c2s" name="c2s" onkeyup="checkSyntax2();" onchange="checkSyntax2();clearSelect();fadeMyErrors();">
        <?php

        if (isset($_POST['c2s']) && !isset($_POST['clearsecond']))
        {
            echo htmlspecialchars($_POST['c2s']);
        }

        ?>
        </textarea><br>
        <div style="height:16px; margin-top:4px;float:left">
            <span style="display:inline-block;height:100%; margin-right:2px">Syntax checking: </span><span id="check2"></span>
        </div>
        <input style="width:120px; margin-top:3px; float:right; border:1px solid #111; border-radius:3px; font-size:8pt; line-height:20px" type="submit" value="CLEAR" name="clearsecond" />
        </td>
        </tr>
        </table><br />
        <center>
        Select an example: 
        <select id="mainselect" style="border:1px solid #111; width:400px; margin-left:10px; margin-bottom:20px; margin-right:110px" onchange="fadeMyErrors();fillContracts();checkSyntax1();checkSyntax2();">
          <option disabled selected>---</option>
          <option value="zip1">Ex. zip code 1: not compliant</option>
          <option value="zip2">Ex. zip code 2: not compliant</option>
          <option value="ex2a">Ex. 2a  : compliant</option>
          <option value="ex2b">Ex. 2b  : not compliant</option>
          <option value="payPal1">Ex. 3 paypal short: compliant</option>
          <option value="ex4">Ex. 4  : compliant</option>
          <option value="ex5a">Ex. 5a  : not compliant</option>
          <option value="ex5b">Ex. 5b  : compliant</option>
          <option value="ex5c">Ex. 5c  : not compliant</option>
          <option value="ex8">Ex. 8  :  compliant</option>
          <option value="exP1">Ex. paypal full Alice:  compliant</option>
          <option value="exP2">Ex. paypal full Bob:  compliant</option>
          <option value="exP2">Ex. 11:  compliant</option>
          <!--<option value="external">External choice</option>
          <option value="recursion">Recursion</option>-->
        </select><br />
        <input type="submit" name="compliance" id="compliancebutton" class="compliancebutton" value="CHECK COMPLIANCE" onclick="<?php 

        if(isset($_SERVER['HTTP_USER_AGENT'])){
            $agent = $_SERVER['HTTP_USER_AGENT'];
            
            if(strlen(strstr($agent,"Firefox")) > 0 ){      
            
                echo "hourGlassFirefox();";
            }
            else
                echo "hourGlass();";
        }
        else
            echo "hourGlass();"; ?>" /></center>

        </form>

        <h2>TST Dualizer</h2>
        <div style="margin-bottom:0px; padding-bottom:0px; font-size:8pt; color:#fff;text-shadow:1px 1px 3px #111">Type your contract here:</div>
        <form method="post" action="index.php">
        <input type="hidden" name="action" value="dual">
        <input type="text" name="contract" size="60">

        <input type="submit" class="compliancebutton" value="GET DUAL" />
        </form>

    </div>
</div>
<div style="margin:auto;margin-top:4px;font-size:8pt; color:#999; font-family:Arial">&copy 2015 Trustworthy Computational Societies, University of Cagliari.</div>
<div style="margin:auto; width:100%; text-align:center; margin-top:20px">
<div id="msgwrap" style="margin:auto; font-size:12pt; font-family:Arial">


<?php

    if ($flag == "dual" && $con!="") {

        if (isset($dual_err)) {
            echo "<div id=\"message\" class=\"message\" style=\"color:#FF0000\">".$dual_err."</div>";
        }
        else if ($dual_c_dual) {
            echo    "<div id=\"message\"  class=\"message\" style=\"color:#009933; text-align:left\">".
                        "Contract: <span style=\"color:#000000; font-family:monospace; font-weight:normal\";>".$dual_c."</span></br>".
                        "Dual: <span style=\"color:#000000; font-family:monospace; font-weight:normal\";>".$dual_c_dual."</span></br>".
                        "<span style=\"color:#000000; font-family:monospace; font-weight:normal\";>".$dual_admit_compl."</span>".
                    "</div>";
        }
    }
    else if ($flag == "compliance") {
        if (isset($response) && !isset($inv_input))
        {
            if (strpos($response,"Property is not satisfied") !== false)
                echo "<div id=\"message\" class=\"message\" style=\"color:#FF0000\">The two contracts are not compliant!</div>";
            else if (strpos($response,"Property is satisfied") !== false)
                echo "<div id=\"message\" class=\"message\" style=\"color:#009933\">The two contracts are compliant!</div>";
            else
                echo "<div id=\"message\"  class=\"message\" style=\"color:#660066\">An error occured: check the log file.</div>";// (".htmlspecialchars($response).").</div>";
        }
        
        if (isset($inv_input))
        {   
            if ($inv_input == 1) {
                echo "<div id=\"error\" class=\"message\" style=\"color:#660066\">You've entered a prohibited symbol in your first contract!</div>";
                
                if ($pos > 10)
                    echo "<div id=\"errordet\" style=\"font-size:10pt; height:30px; font-weight:normal\" class=\"message\"><b>Error details</b>: the invalid symbol '<i>".substr($_POST['c1s'], $pos, 1)."</i>' can be found after '<i>".substr($_POST['c1s'], $pos-10, 10)."</i>'.</div>";
                else
                    echo "<div id=\"errordet\" style=\"font-size:10pt; height:30px; font-weight:normal\" class=\"message\"><b>Error details</b>: the invalid symbol '<i>".substr($_POST['c1s'], $pos, 1)."</i>' can be found at the beginning of the contract.";
            }
            else if ($inv_input == 2)  {
                echo "<div id=\"error\" class=\"message\" style=\"color:#660066\">You've entered a prohibited symbol in your second contract!</div>";
                
                if ($pos > 10)
                    echo "<div id=\"errordet\" style=\"font-size:10pt; height:30px; font-weight:normal\" class=\"message\"><b>Error details</b>: the invalid symbol '<i>".substr($_POST['c2s'], $pos, 1)."</i>' can be found after '<i>".substr($_POST['c2s'], $pos-10, 10)."</i>'.</div>";
                else
                    echo "<div id=\"errordet\" style=\"font-size:10pt; height:30px; font-weight:normal\" class=\"message\"><b>Error details</b>: the invalid symbol '<i>".substr($_POST['c2s'], $pos, 1)."</i>' can be found at the beginning of the contract.";
            }
            else if ($inv_input == 3)
                echo "<div id=\"error\" class=\"message\" style=\"color:#660066\">Why do you want to check compliance for empty contracts?</div>";
            else if ($inv_input == 4)
                echo "<div id=\"error\" class=\"message\" style=\"color:#660066\">Input too long, operation canceled.</div>";
            else if ($inv_input == 5) {
            
                echo "<div id=\"error\" class=\"message\" style=\"color:#660066\">There's a syntax error in your first string contract!</div>";
                echo "<div id=\"errordet\" style=\"font-size:10pt; height:30px; font-weight:normal\" class=\"message\"><b>Error details</b>: ".errorFilter($translate1)."</div>";
            }
            else if ($inv_input == 6) {
            
                echo "<div id=\"error\" class=\"message\" style=\"color:#660066\">There's a syntax error in your second string contract!</div>";
                echo "<div id=\"errordet\" style=\"font-size:10pt; height:30px; font-weight:normal\" class=\"message\"><b>Error details</b>: ".errorFilter($translate2)."</div>";
            }
            else if ($inv_input == 7) {
            
                echo "<div id=\"error\" class=\"message\" style=\"color:996600\">Your first contract is empty!</div>";
            }
            else if ($inv_input == 8) {
            
                echo "<div id=\"error\" class=\"message\" style=\"color:996600\">Your second contract is empty!</div>";
            }
            
            unset($inv_input);
        }
    }
?>
</div>
</div>
</div>
</body>
</html>
