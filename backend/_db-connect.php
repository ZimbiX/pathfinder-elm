<?php
$db_config = $config['database'];

mysqli_report(MYSQLI_REPORT_ERROR | MYSQLI_REPORT_STRICT);

// connect to the database
$con = mysqli_connect(
  $db_config['server'],
  $db_config['username'],
  $db_config['password'],
  $db_config['db_name']
);
?>
