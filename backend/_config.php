<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

$config = json_decode(file_get_contents('config.json'), true);
?>
