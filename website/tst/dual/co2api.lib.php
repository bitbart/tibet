<?php

function newLineFilter($string) {

    return str_replace(PHP_EOL, ' ', $string);
}


function translate_contract($input) {

    $url = 'http://co2.unica.it:8080/server/api/translate';
    
    $params = array(
                    'c1' => str_replace(PHP_EOL, '', $input)
                );
    
    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, 1);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, http_build_query($params));
    curl_setopt( $ch, CURLOPT_FOLLOWLOCATION, 1);
    curl_setopt( $ch, CURLOPT_HEADER, 0);
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1);
    
    return curl_exec( $ch );
}

function areCompliant($c1, $c2) {
    
    $url = 'http://co2.unica.it:8080/server/api/areCompliant';
            
    $params = array(
                    'c1' => str_replace(PHP_EOL, '', $c1),
                    'c2' => str_replace(PHP_EOL, '', $c2)
                );
    
    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, 1);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, http_build_query($params));
    curl_setopt( $ch, CURLOPT_FOLLOWLOCATION, 1);
    curl_setopt( $ch, CURLOPT_HEADER, 0);
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1);
    
    return curl_exec( $ch );
}

function tellContract($user, $pass, $contract, $nonce) {
    
    $url = 'http://co2.unica.it:8080/server/api/handling/tellContract';
            
    $params = array(
                    'username' => $user,
                    'pass' => hash("sha256", $pass),
                    'contract' => $contract,
                    'timestamp' => $nonce
                );
    
    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, 1);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, http_build_query($params));
    curl_setopt( $ch, CURLOPT_FOLLOWLOCATION, 1);
    curl_setopt( $ch, CURLOPT_HEADER, 0);
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1);
    
    return curl_exec( $ch );
}

function admitsCompliant($contract) {
    
    $url = 'http://co2.unica.it:8080/server/api/dualize/admitsCompliant';
            
    $params = array(
                    'contract' => str_replace(PHP_EOL, '', $contract)
                );
    
    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, 1);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, http_build_query($params));
    curl_setopt( $ch, CURLOPT_FOLLOWLOCATION, 1);
    curl_setopt( $ch, CURLOPT_HEADER, 0);
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1);
    
    return curl_exec( $ch );
}

function dualOf($contract) {
    
    $url = 'http://co2.unica.it:8080/server/api/dualize/dualof';
            
    $params = array(
                    'contract' => str_replace(PHP_EOL, '', $contract)
                );
    
    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, 1);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, http_build_query($params));
    curl_setopt( $ch, CURLOPT_FOLLOWLOCATION, 1);
    curl_setopt( $ch, CURLOPT_HEADER, 0);
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1);
    
    return curl_exec( $ch );
}

function kindOf($contract) {
    
    $url = 'http://co2.unica.it:8080/server/api/dualize/kindof';
            
    $params = array(
                    'contract' => str_replace(PHP_EOL, '', $contract)
                );
    
    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, 1);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, http_build_query($params));
    curl_setopt( $ch, CURLOPT_FOLLOWLOCATION, 1);
    curl_setopt( $ch, CURLOPT_HEADER, 0);
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1);
    
    return curl_exec( $ch );
}

function hasErrors($contract) {

    if (strpos($contract, "<contract>") === false)
        return true;
        
    return false;
} 

function errorFilter($string) {

    $string = newLineFilter($string);

    $s1 = str_replace("<xml><response type=\"error\">Fatal error: exception Stream.Error(\"", "", $string);
    $s2 = str_replace("\") </response></xml>", "", $s1);
    $s3 = str_replace("<xml><response type=\"error\">Fatal error: exception Failure(\"", "", $s2);
    
    return substr($s3, 9, strlen($s3) - 8);
}

function startsWith($haystack, $needle) {

    return $needle === "" || strrpos($haystack, $needle, -strlen($haystack)) !== FALSE;
}

?>