<?php

function newLineFilter($string) {

    return str_replace(PHP_EOL, ' ', $string);
}


function dualOf($contract) {

    $url = 'http://co2.unica.it:8080/middleware/api/dualize/dualof';

    $data = array(
                    'firstContract' => str_replace(PHP_EOL, '', $contract)
                );
    $params = json_encode($data);

    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, true);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, $params);
    curl_setopt( $ch, CURLOPT_HTTPHEADER, array("Content-type: application/json"));
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true);

    $json_response = curl_exec($ch);
    $status = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);

    $response = json_decode($json_response,true);
    return $response["content"];
}



function translate_contract($input) {

    $url = 'http://co2.unica.it:8080/middleware/api/translation/translate';

    $data = array(
                    'firstContract' => str_replace(PHP_EOL, '', $input)
                );

    $params = json_encode($data);

    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, true);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, $params);
    curl_setopt( $ch, CURLOPT_HTTPHEADER, array("Content-type: application/json"));
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true);

    $json_response = curl_exec($ch);
    $status = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);

    $response = json_decode($json_response,true);
    return $response["content"];
}


function areCompliant($c1, $c2) {

    $url = 'http://co2.unica.it:8080/middleware/api/complaince/areCompliant';
 
    $data = array(
                    'firstContract' => str_replace(PHP_EOL, '', $c1),
                    'secondContract' => str_replace(PHP_EOL, '', $c2)
                );

    $params = json_encode($data);

    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, true);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, $params);
    curl_setopt( $ch, CURLOPT_HTTPHEADER, array("Content-type: application/json"));
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true);

    $json_response = curl_exec($ch);
    $status = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);

    $response = json_decode($json_response,true);
    return $response["content"];
}

/*
function tellContract($user, $pass, $contract, $nonce) {

    $url = 'http://co2.unica.it:8080/middleware/api/handling/tellContract';

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
}*/

function admitsCompliant($contract) {

    $url = 'http://co2.unica.it:8080/middleware/api/dualize/admitsCompliant';


    $data = array(
                    'firstContract' => str_replace(PHP_EOL, '', $contract)
                );
    $params = json_encode($data);

    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, true);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, $params);
    curl_setopt( $ch, CURLOPT_HTTPHEADER, array("Content-type: application/json"));
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true);

    $json_response = curl_exec($ch);
    $status = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);

    $response = json_decode($json_response,true);
    return $response["content"];
}

function kindOf($contract) {

    $url = 'http://co2.unica.it:8080/middleware/api/dualize/kindof';


    $data = array(
                    'firstContract' => str_replace(PHP_EOL, '', $contract)
                );
    $params = json_encode($data);

    $ch = curl_init( $url );
    curl_setopt( $ch, CURLOPT_POST, true);
    curl_setopt( $ch, CURLOPT_POSTFIELDS, $params);
    curl_setopt( $ch, CURLOPT_HTTPHEADER, array("Content-type: application/json"));
    curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true);

    $json_response = curl_exec($ch);
    $status = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);

    $response = json_decode($json_response,true);
    return $response["content"];

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
