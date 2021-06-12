<?php
$db_config = $config['database'];

// connect to the database
$con = mysqli_connect(
  $db_config['server'],
  $db_config['username'],
  $db_config['password'],
  $db_config['db_name']
);

// check connection
if (mysqli_connect_errno($con)) {
  echo "Failed to connect to MySQL: " . mysqli_connect_error();
}
?>
