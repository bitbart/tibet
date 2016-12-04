<?php

function newLineFilter($string) {

    return str_replace(PHP_EOL, ' ', $string);
}


function translate_contract($input) {

    $url = 'http://localhost:8080/middleware/api/translation/translate';
    
    //$params = array(
    //                'firstContract' => str_replace(PHP_EOL, '', $input)
    //            );
    
    $data = array("firstContract" => str_replace(PHP_EOL, '', $input));                                                                    
    $params = json_encode($data); 
    
    $ch = curl_init( $url );
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
    curl_setopt( $ch, CURLOPT_POSTFIELDS, $params);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_HTTPHEADER, array(                                                                          
    'Content-Type: application/json',                                                                                
    'Content-Length: ' . strlen($params))                                                                       
    ); 
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1);
    $result = curl_exec( $ch );
    $array = json_decode($result, true);
    
    return $array["content"];
}

function areCompliant($c1, $c2) {
    
    $url = 'http://localhost:8080/middleware/api/compliance/areCompliant';
            
    $data = array(
                    'firstContract' => str_replace(PHP_EOL, '', $c1),
                    'secondContract' => str_replace(PHP_EOL, '', $c2)
                );
    
    $params = json_encode($data); 
    
    $ch = curl_init( $url );
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
    curl_setopt( $ch, CURLOPT_POSTFIELDS, $params);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_HTTPHEADER, array(                                                                          
    'Content-Type: application/json',                                                                                
    'Content-Length: ' . strlen($params))                                                                       
    ); 
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1);
    $result = curl_exec( $ch );
    $array = json_decode($result, true);
    
    return $array["content"];
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

?>