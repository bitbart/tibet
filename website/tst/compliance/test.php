<?php

preg_match("/(?![a-zA-Z0-9\?\"\[\]\<\>\!\s\{\}\,\;\.\&\+\)\(]+)/", "ciao_ciao", $matches, PREG_OFFSET_CAPTURE);
echo $matches[0][1];

?>