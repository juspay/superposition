use leptos::*;
use leptos_router::use_params_map;

#[component]
pub fn experiment_page() -> impl IntoView {
    let exp_params = use_params_map();
    let experiment_id =
        move || exp_params.with(|params| params.get("id").cloned().unwrap());

    view! {
        <div class="flex flex-col overflow-x-auto p-2">
        <h1 class="text-4xl pt-4 font-extrabold">
            Experiment Name
            <span class="badge ml-3 mb-1 badge-primary badge-lg">Created</span>
        </h1>

        <div class="divider"></div>

        <div class="join m-5">
            <button class="btn join-item"><i class="ri-edit-line"></i>Edit</button>
            <button class="btn join-item"><i class="ri-stop-circle-line"></i>Conclude</button>
            <button class="btn join-item"><i class="ri-guide-line"></i>Release</button>
            <button class="btn join-item"><i class="ri-flight-takeoff-line"></i>Ramp</button>
        </div>

        <div class="stats bg-primary shadow-xl mt-5 text-primary-content">
            <div class="stat">
                <div class="stat-title">Experiment ID</div>
                <div class="stat-value">{experiment_id}</div>
            </div>
            <div class="stat">
                <div class="stat-title">Current Traffic Percentage</div>
                <div class="stat-value">0</div>
            </div>
            <div class="stat">
                <div class="stat-title">Created at</div>
                <div class="stat-value">19/11/2023</div>
            </div>
            <div class="stat">
                <div class="stat-title">Created by</div>
                <div class="stat-value">mobius@juspay.in</div>
            </div>
        </div>

        <div class="card bg-base max-w-screen shadow-xl mt-5">
            <div class="card-body">
                <h2 class="card-title">Context</h2>
                <div class="flex flex-row">
                    <div class="stat">
                        <div class="stat-title">Client ID</div>
                        <div class="stat-value">cac</div>
                    </div>
                    <div class="divider divider-horizontal">&&</div>
                    <div class="stat">
                        <div class="stat-title">OS</div>
                        <div class="stat-value">android</div>
                    </div>
                </div>
            </div>
            </div>


            <div class="card bg-base max-w-screen shadow-xl mt-5">
            <div class="card-body">
                <h2 class="card-title">Variants</h2>
                <div class="overflow-x-auto">
                <table class="table">
                    <thead>
                    <tr>
                        <th></th>
                        <th>Key</th>
                        <th>Variant-1</th>
                        <th>Variant-2</th>
                        <th>Variant-3</th>
                        <th>Variant-4</th>
                        <th>Variant-5</th>
                        <th>Control</th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr>
                        <th>1</th>
                        <td>pmTestKey1</td>
                        <td>Quality Control Specialist</td>
                        <td>Quality Control Specialist</td>
                        <td>Quality Control Specialist</td>
                        <td>Quality Control Specialist</td>
                        <td>Blue</td>
                        <td>Blue</td>
                    </tr>
                    <tr>
                        <th>2</th>
                        <td>pmTestKey2</td>
                        <td>Desktop Support Technician</td>
                        <td>Desktop Support Technician</td>
                        <td>Desktop Support Technician</td>
                        <td>Desktop Support Technician</td>
                        <td>Desktop Support Technician</td>
                        <td>Purple</td>
                    </tr>
                    <tr>
                        <th>3</th>
                        <td>pmTestKey3</td>
                        <td>Tax Accountant</td>
                        <td>Tax Accountant</td>
                        <td>Tax Accountant</td>
                        <td>Tax Accountant</td>
                        <td>Tax Accountant</td>
                        <td>Red</td>
                    </tr>
                    </tbody>
                </table>
                </div>
            </div>
        </div>
        </div>
    }
}
