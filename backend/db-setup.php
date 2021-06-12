<?php
require('_config.php');
require('_db-connect.php');

function reset_db() {
  global $con;
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
}

if (isset($_GET['reset'])) {
  if (isset($_POST['password']) && $_POST['password'] === $config['admin']['password']) {
    echo 'Resetting database&hellip;<br/>';

    reset_db();

    echo 'Database has been reset.';
  } else {
    echo 'Admin password wrong.';
  }
} else {
?>
<form action="db-setup.php?reset" method="POST">
  <p>Enter password to reset the database:</p>
  <input type="text" name="password" />
  <input type="submit" />
</form>
<?php
}
?>
