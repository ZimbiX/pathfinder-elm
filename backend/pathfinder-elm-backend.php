<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

include('_db-connect.php');

header('Cache-Control: no-store');
header('Access-Control-Allow-Origin: *');

function getIpInfo() {
  $ip_info = [
    'HTTP_CLIENT_IP' => isset($_SERVER['HTTP_CLIENT_IP']) ? $_SERVER['HTTP_CLIENT_IP'] : null,
    'HTTP_X_FORWARDED_FOR' => isset($_SERVER['HTTP_X_FORWARDED_FOR']) ? $_SERVER['HTTP_X_FORWARDED_FOR'] : null,
    'HTTP_X_FORWARDED' => isset($_SERVER['HTTP_X_FORWARDED']) ? $_SERVER['HTTP_X_FORWARDED'] : null,
    'HTTP_FORWARDED_FOR' => isset($_SERVER['HTTP_FORWARDED_FOR']) ? $_SERVER['HTTP_FORWARDED_FOR'] : null,
    'HTTP_FORWARDED' => isset($_SERVER['HTTP_FORWARDED']) ? $_SERVER['HTTP_FORWARDED'] : null,
    'REMOTE_ADDR' => isset($_SERVER['REMOTE_ADDR']) ? $_SERVER['REMOTE_ADDR'] : null,
  ];
  return $ip_info;
}

function store_event_for_id() {
  global $con;
  $id = $_POST['id'];
  $event = $_POST['event'];
  $version = intval($_POST['version']);
  $ip_info = json_encode(getIpInfo());

  $stmt = $con->prepare("insert into Store (id, event, version, ip_info) values (?, ?, ?, ?)");
  $stmt->bind_param("ssis", $id, $event, $version, $ip_info);
  $result = $stmt->execute();
  if ($result) {
    http_response_code(201);
    echo "ok";
  } else {
    http_response_code(409); // HTTP 409 Conflict
    echo "bad";
  }
}

function row_with_decoded_event($row) {
  if (isset($_GET['id']) && strpos($_GET['id'], 'invalid-') === 0) {
    $row["event"] = "Jim's Invalid Events";
  } else {
    $row["event"] = json_decode($row["event"]);
  }
  if (isset($row["ip_info"])) {
    $row["ip_info"] = json_decode($row["ip_info"]);
  }
  return $row;
}

function get_events_after_version() {
  global $con;
  $id = $_GET['id'];
  if (isset($_GET['after'])) {
    $after_version = intval($_GET['after']);
  } else {
    $after_version = 0;
  }

  $sql = "select version, event, date_format(convert_tz(at, 'SYSTEM', 'UTC'), '%Y-%m-%dT%TZ') as at" .
    (isset($_GET['ip']) ? ", ip_info" : "") .
    " from Store where id = ? and version > ? order by version";

  $stmt = $con->prepare($sql);
  $stmt->bind_param("si", $id, $after_version);
  $stmt->execute();
  $result = $stmt->get_result()->fetch_all(MYSQLI_ASSOC);

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

function get_all_events() {
  global $con;
  $sql = "select id, version, event, date_format(convert_tz(at, 'SYSTEM', 'UTC'), '%Y-%m-%dT%TZ') as at" .
    (isset($_GET['ip']) ? ", ip_info" : "") .
    " from Store";

  $stmt = $con->prepare($sql);
  $stmt->execute();
  $result = $stmt->get_result()->fetch_all(MYSQLI_ASSOC);

  if ($result) {
    echo json_encode(array_map("row_with_decoded_event", $result));
  } else {
    echo json_encode([]);
  }
}

if (!empty($_POST)) {
  if (strpos($_POST['id'], 'error-') === 0) {
    http_response_code(500);
    echo "fake bad";
  } else {
    store_event_for_id();
  }
} else if (isset($_GET['id']) && strpos($_GET['id'], 'error-') === 0 && $_GET['after'] != '0') {
  http_response_code(500);
  echo "fake bad";
} else if (!isset($_GET['id']) && isset($_GET['all'])) {
  get_all_events();
} else if (isset($_GET['id']) && isset($_GET['after'])) {
  get_events_after_version();
} else {
  header('Location: https://pathfinder-elm.netlify.app/');
}
?>
