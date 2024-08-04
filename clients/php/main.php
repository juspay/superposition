<?php
    require_once "./cacclient/client.php";
    require_once "./expclient/client.php";

    $tenant_name = "dev";
    $polling_frequency = 1;
    $cac_host_name = "http://localhost:8080";

    $cac_client = new CACClient($tenant_name, $polling_frequency, $cac_host_name);
    $exp_client = new ExperimentationClient($tenant_name, $polling_frequency, $cac_host_name);

    $cac_client->create_new_cac_client();
    $exp_client->create_expt_new_client();

    $cac_client-> start_cac_polling_update();
    sleep(5);
    $resp_cac = $cac_client->get_cac_config("{}", "");
    $resp_exp = $exp_client->get_expt_running_experiments();
    echo $resp_cac;
    echo "\n";
    echo $resp_exp;
?>