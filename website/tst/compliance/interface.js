function checkSyntax1(contract)
{
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
        
            if (xmlhttp.responseText == "y")
                document.getElementById("check1").innerHTML = "<img src='yes2.png' style='width:24px; height:24px; vertical-align:middle' />";
            else
                document.getElementById("check1").innerHTML = "<img src='no2.png' style='width:24px; height:24px; vertical-align:middle' />";
        }
    }
    xmlhttp.open("POST", "check_syntax.php", true);
    xmlhttp.setRequestHeader("Content-type","application/x-www-form-urlencoded");
    xmlhttp.send("contract=" + encodeURIComponent(document.getElementById('c1s').value));
}

function checkSyntax2(contract)
{
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
        
            if (xmlhttp.responseText == "y")
                document.getElementById("check2").innerHTML = "<img src='yes2.png' style='width:24px; height:24px; vertical-align:middle' />";
            else
                document.getElementById("check2").innerHTML = "<img src='no2.png' style='width:24px; height:24px; vertical-align:middle' />";
        }
    }
    xmlhttp.open("POST", "check_syntax.php", true);
    xmlhttp.setRequestHeader("Content-type","application/x-www-form-urlencoded");
    xmlhttp.send("contract=" + encodeURIComponent(document.getElementById('c2s').value));
}

function clearSelect() 
{
    document.getElementById("mainselect").options.selectedIndex = 0;
}

function fillContracts() {

    var sel = document.getElementById("mainselect").options.selectedIndex;
    
    if (sel == 1) { //zip code example not compliant
    
        document.getElementById("c1s").value = "?zip{;x}.(!weather{x>5, x<10} + !abort{x<1})";
        document.getElementById("c2s").value = "!zip{;y}.(?weather{y<7} & ?abort{y<5})";
    }
    else if (sel == 2) { //zip code example  not compliant
    
        document.getElementById("c1s").value = "?zip{;x}.(!weather{x>5, x<10} + !abort{x<1})";
        document.getElementById("c2s").value = "!zip{y<10}.(?weather{y<7} & ?abort{y<5})";
    }
    else if (sel == 3) { //ex 2A
    
        document.getElementById("c1s").value = "?a{t<5}.!b{t<3}";
        document.getElementById("c2s").value = "!a{t<2}.?b{t<3}";
    }
    else if (sel == 4) { //ex 2B
    
        document.getElementById("c1s").value = "?a{t<5}.!b{t<3}";
        document.getElementById("c2s").value = "!a{t<5}.?b{t<3}";
    }
    else if (sel == 5) { //paypal example 3 
    
        document.getElementById("c1s").value = "?pay{;t}.(?ok & ?dispute{t < 180; t}.(?ok{t<20} & ?claim{t<20, t>7;t}.?rcpt{t<3;t}.!refund{t<7} & ?abort))";
        document.getElementById("c2s").value = "!pay{;t}.(!ok{t>10,t<11} + !dispute{t>10,t<11; t}.!claim{t>10,t<11}.!rcpt{t>10, t<11}.?refund)";
    }
    else if (sel == 6) { //rec example 4  
    
        document.getElementById("c1s").value = "REC 'x' [!a + !b{x<2}.?c.'x']";
        document.getElementById("c2s").value = "?a & ?b{y<2}.!c{y>2}.?a";
    }
    else if (sel == 7) { //ex 5a  
    
        document.getElementById("c1s").value = "!a{x<3}.!b{x<2}";
        document.getElementById("c2s").value = "?a{x<3}.?b{x<2}";
    }
    else if (sel == 8) { //ex 5b  
    
        document.getElementById("c1s").value = "!a{x<2}.!b{x<2}";
        document.getElementById("c2s").value = "?a{x<3}.?b{x<2}";
    }
    else if (sel == 9) { //ex 5c  
    
        document.getElementById("c1s").value = "!a{x<3}+!b{x<2}.?a{x<1}";
        document.getElementById("c2s").value = "?a{x<3}&?b{x<2}.!a{x<1}";
    }
    else if (sel == 10) { //ex 8  
    
        document.getElementById("c1s").value = "!a{t>2,t<4}";
        document.getElementById("c2s").value = "?a{t>2,t<5}&?b{t>2,t<5}";
    }
    else if (sel == 11) { //ex paypal  
    
        document.getElementById("c1s").value =  "?pay{; tp}.( ?ok \n"
               +"  & ?inr{tp<180;tinr}.( ?ok{tinr<20} \n"
               +"      & ?claiminr{tinr<20,tp > 7; tc}. \n"
               +"          ?receipt{tc<3;tc}.!refund{tc<7} \n"
               +"      & ?claimsnad{tinr< 20; tc}.?photo{tc<7}.( \n"
               +"           !sendback{;tc}.?acksendback{tc<3}.!refund \n"
               +"          +!destroy{;tc}.?ackdestroy{tc<3}.!refund) \n"              
               +"      & ?abort)   \n"
               +"  & ?snad{tp<180;tsnad}.(?ok{tsnad<20}  \n"
               +"      & ?claimsnad{tsnad<20;tc}.?photo{tc<7}.( \n"
               +"           !sendback{;tc}.?acksendback{tc<3}.!refund \n"
               +"          +!destroy{;tc}.?ackdestroy{tc<3}.!refund) \n"
               +"      & ?abort)"
               +" \n)";
        document.getElementById("c2s").value = "!pay{; tp}.( !ok{tp<10} \n"
               +" + !inr{tp>10,tp<11}. \n"
               +"      !claiminr{tp>10,tp<11}. \n"
               +"           !receipt{tp>10,tp<11}.?refund \n"      
               +" + !snad{tp<10;tsnad}.  \n"
               +"      !claimsnad{tsnad<1}.!photo{tsnad<1}.(\n"
               +"         ?sendback{;tc}.!acksendback{tc<3}.?refund \n"
               +"       & ?destroy{;tc}.!ackdestroy{tc<3}.?refund  ) \n"
               +")";
    }
    else if (sel == 12) { //ex paypal  
    
        document.getElementById("c1s").value =  "?pay{; tp}.( ?ok \n"
               +"  & ?inr{tp<180;tinr}.( ?ok{tinr<20} \n"
               +"      & ?claiminr{tinr<20,tp > 7; tc}. \n"
               +"          ?receipt{tc<3;tc}.!refund{tc<7} \n"
               +"      & ?claimsnad{tinr< 20; tc}.?photo{tc<7}.( \n"
               +"           !sendback{;tc}.?acksendback{tc<3}.!refund \n"
               +"          +!destroy{;tc}.?ackdestroy{tc<3}.!refund) \n"              
               +"      & ?abort)   \n"
               +"  & ?snad{tp<180;tsnad}.(?ok{tsnad<20}  \n"
               +"      & ?claimsnad{tsnad<20;tc}.?photo{tc<7}.( \n"
               +"           !sendback{;tc}.?acksendback{tc<3}.!refund \n"
               +"          +!destroy{;tc}.?ackdestroy{tc<3}.!refund) \n"
               +"      & ?abort)"
               +" \n)";
        document.getElementById("c2s").value = "!pay{; tp}.( !ok{tp<10} \n"
               +" + !inr{tp>10,tp<11;tinr}.( \n"
               +"       !ok{tinr<4}  \n" 
               +"     + !claiminr{tinr>4,tinr<5; tc}. \n"
               +"                  !receipt{tc<3;tc}.?refund \n"
               +"     + !claimsnad{tinr<4; tc}.!photo{tinr<7}.( \n"
               +"          ?sendback{;tc}.!acksendback{tc<3}.?refund \n"
               +"        & ?destroy{;tc}.!ackdestroy{tc<3}.?refund )) \n"               
               +" + !snad{tp<10;tsnad}.( !ok{tsnad<4} \n"
               +"     + !claimsnad{tsnad<4;tc}.!photo{tc<7}.(\n"
               +"         ?sendback{;tc}.!acksendback{tc<1}.?refund \n"
               +"       & ?destroy{;tc}.!ackdestroy{tc<1}.?refund  )) \n"
               +")";
    }
    else if (sel == 13) { //ex 8  
    
        document.getElementById("c1s").value = "REC 'x' [!a{c>2,c<3;c}.'x' + !b{t<7}]";
        document.getElementById("c2s").value = "REC 'y' [?a{r>1,r<5;r}.'y' & ?b{r<7}]";
    }
    else if (sel == 14) { //ex clash nomi  
    
        document.getElementById("c1s").value = "!a{x < 1,x > 0;y}.\n"
                                    +"    REC 'x' [\n"
                                    +"             !a{x < 1,x > 0,y > 1;y}.'x' \n"
                                    +"           + !b{y < 1,y > 0,x > 1;x}.'x'\n"
                                    +"    ]";
        document.getElementById("c2s").value = "REC 'x' [?a.?b.'x']";
    }
}

function hourGlass() {

    document.getElementById('msgwrap').innerHTML = '<div id=\'message\' class=\'message\' style=\'border:none; background:none\'><img src=\'hourglass.gif\' alt=\'Loading...\' width=\'20\' height=\'20\' /></div>';
}

function hourGlassFirefox() {

    document.getElementById('msgwrap').innerHTML = '<div id=\'message\' class=\'message\' style=\'border:none; background:none\'><img src=\'hourglass.png\' alt=\'Loading...\' width=\'20\' height=\'20\' /></div>';
}

$(document).ready(function() {
   window.setTimeout("fadeMyDiv();", 4000);
 }
)

function fadeMyDiv() {
   $("#message").fadeOut(3000);
}
function fadeMyErrors() {
   $("#error").fadeOut(1000);
   $("#errordet").fadeOut(1000);
}
