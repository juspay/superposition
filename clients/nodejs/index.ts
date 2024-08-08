import express, {
    Request,
    Response
} from 'express';
import CacClient from 'cac_client';
import ExperimentationClient from 'experimentation_client';

const app = express();
const port = process.env.PORT || 7000;

let tenantName: string = "dev";
let superpositionHost: string = "http://localhost:8080";
let pollingFrequency: number = 1;

let CACClient = new CacClient(tenantName, pollingFrequency, superpositionHost);
let ExpClient = new ExperimentationClient(tenantName, pollingFrequency, superpositionHost);

ExpClient.createNewExperimentaionClient();
CACClient.createNewCACClient();

(async function () {
    ExpClient.startExperimantionPollingUpdate();
    CACClient.startCACPollingUpdate();
})();

app.get('/', (req: Request, res: Response) => {
    let resp2 = CACClient.getDefaultConfig("");
    let resp3 = ExpClient.getRunningExpriments();
    res.send({resp2, resp3});
});

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
});