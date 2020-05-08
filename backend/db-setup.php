<?php
if (isset($_GET["reset"])) {
  echo 'Resetting database&hellip;<br/>';

  require('_db-connect.php');

  mysqli_query($con,"DROP TABLE Store"); // remove table if it already exists
  $command = "CREATE TABLE Store
    (
      id CHAR(36) NOT NULL,
      event TEXT NOT NULL,
      version INT NOT NULL,
      at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      ip_info TEXT NOT NULL,
      PRIMARY KEY(id, version)
    )";
  if (!mysqli_query($con,$command)) {
    exit("Failed to create Store table: " . mysqli_error($con));
  }

  echo 'Database has been reset.';
} else {
  echo 'I confirm I want to <a href="db-setup.php?reset">reset the database</a>';
}
?>
