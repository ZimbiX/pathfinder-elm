<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

include('_db-connect.php');

header('Cache-Control: no-store');
header('Access-Control-Allow-Origin: *');

function store_event_for_id() {
  global $con;
  $id = $_POST['id'];
  $event = $_POST['event'];
  $version = intval($_POST['version']);

  $stmt = $con->prepare("insert into Store (id, event, version) values (?, ?, ?)");
  $stmt->bind_param("ssi", $id, $event, $version);
  $result = $stmt->execute();
  if ($result) {
    http_response_code(201);
    echo "ok";
  } else {
    http_response_code(409); // HTTP 409 Conflict
    echo "bad";
  }
}

function get_events_after_version() {
  global $con;
  $id = $_GET['id'];
  if (isset($_GET['after'])) {
    $after_version = intval($_GET['after']);
  } else {
    $after_version = 0;
  }

  $stmt = $con->prepare("select version, event, date_format(convert_tz(at, 'SYSTEM', 'UTC'), '%Y-%m-%dT%TZ') as at from Store where id = ? and version > ? order by version");
  $stmt->bind_param("si", $id, $after_version);
  $stmt->execute();
  $result = $stmt->get_result()->fetch_all(MYSQLI_ASSOC);

  function row_with_decoded_event($row) {
    $row["event"] = json_decode($row["event"]);
    return $row;
  }

  function only_contiguous_versions($after_version, $events) {
    $contiguous = [];
    $expected_next_version = $after_version;
    foreach ($events as $row) {
      $expected_next_version++;
      if ($row['version'] != $expected_next_version) { break; }
      array_push($contiguous, $row);
    }
    return $contiguous;
  }

  if ($result) {
    if (isset($_GET['all'])) {
      echo json_encode(array_map("row_with_decoded_event", $result));
    } else {
      echo json_encode(only_contiguous_versions($after_version, array_map("row_with_decoded_event", $result)));
    }
  } else {
    echo json_encode([]);
  }
}

if (!empty($_POST)) {
  store_event_for_id();
} else {
  get_events_after_version();
}
?>
