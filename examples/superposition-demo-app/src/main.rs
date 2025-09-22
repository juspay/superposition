#![deny(unused_crate_dependencies)]
use actix_files as fs;
use actix_web::{get, web, App, HttpResponse, HttpServer, Responder};
use awc::Client;
use serde::Deserialize;
use std::fmt;
use std::str;

#[derive(Debug, Deserialize)]
pub enum Currency {
    INR,
    USD,
    EUR,
}

#[derive(Debug, Deserialize)]
pub enum DistanceUnit {
    Miles,
    Km,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Vehicle {
    Cab,
    Auto,
}

impl fmt::Display for Currency {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Currency::INR => write!(f, "INR"),
            Currency::USD => write!(f, "USD"),
            Currency::EUR => write!(f, "EUR"),
        }
    }
}

impl fmt::Display for DistanceUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DistanceUnit::Km => write!(f, "kilometres"),
            DistanceUnit::Miles => write!(f, "miles"),
        }
    }
}

impl fmt::Display for Vehicle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Vehicle::Cab => write!(f, "cab"),
            Vehicle::Auto => write!(f, "auto"),
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct Context {
    city: Option<String>,
    vehicle_type: Option<Vehicle>,
    hour_of_day: Option<u8>,
}

#[derive(Debug, Deserialize)]
pub struct Config {
    base_rate: f64,
    per_distance_unit_rate: f64,
    distance_unit: DistanceUnit,
    currency: Currency,
    hello_message: String,
    hello_message_color: String,
    logo: String,
}

#[get("/fragments/app.html")]
async fn app(ctx: web::Query<Context>) -> impl Responder {
    let city = ctx.city.clone().unwrap_or("Bangalore".to_string());
    let context = ctx.into_inner();
    let vehicle_type = context.vehicle_type.unwrap_or(Vehicle::Cab);
    let hour_of_day = context.hour_of_day.unwrap_or(0);
    let client = Client::default();
    let req = client.get(format!("http://localhost:8080/config/resolve?dimension[city]={city}&dimension[vehicle_type]={vehicle_type}&dimension[hour_of_day]={hour_of_day}"))
        .insert_header(("x-tenant", "dev"));
    // println!("{}", format!("http://localhost:8080/config/resolve?dimension[city]={city}&dimension[vehicle_type]={vehicle_type}&dimension[hour_of_day]={hour_of_day}"));
    let mut res = req.send().await.expect("Failed to send request");
    let res_string = res
        .body()
        .await
        .expect("Failed to read response body")
        .to_vec();
    let config: Config = serde_json::from_slice(&res_string).unwrap();

    let table = format!(
        r#"
    <style>
      .blink {{
        animation: blinker 2s linear infinite;
        color: {color};
      }}

      @keyframes blinker {{
        50% {{
          opacity: 0;
        }}
      }}

      .logo {{
        display: inline;
      }}
    </style>

    <table class="table-auto border-separate border-spacing-2 border border-slate-400">
      <h2 class="my-4 text-xl sm:text-2xl text-slate-700 tracking-tight">
        Fare for city: <span class="font-bold">{city}</span> - <span class="blink">{message}</span>
        <img class="logo pl-4" height="100px" width="200px" src="{logo}">
      </h2>
      <thead>
        <tr>
          <th class="p-4 border border-slate-300" scope="col">Component</th>
          <th class="p-4 border border-slate-300" scope="col">Charge</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td class="p-4 border border-slate-300" scope="row">Base rate</td>
          <td class="p-4 border border-slate-300">{currency} {base_rate}</td>
        </tr>
        <tr>
          <td class="p-4 border border-slate-300" scope="row">Per KM rate</td>
          <td class="p-4 border border-slate-300">{currency} {per_distance_unit_rate}</td>
        </tr>
        <tr>
          <td class="p-4 border border-slate-300" scope="row">Total fare for 10 {distance_unit} ride</td>
          <td class="p-4 border border-slate-300">{currency} {total_fare}</td>
        </tr>
      </tbody>
      <tfoot>
      </tfoot>
    </table>"#,
        currency = config.currency,
        base_rate = config.base_rate,
        per_distance_unit_rate = config.per_distance_unit_rate,
        total_fare = (config.base_rate + 10.0 * config.per_distance_unit_rate),
        distance_unit = config.distance_unit,
        message = config.hello_message,
        color = config.hello_message_color,
        logo = config.logo
    );

    // HttpResponse::Ok().body()
    HttpResponse::Ok().body(table)
}

#[get("/")]
async fn home(ctx: web::Query<Context>) -> impl Responder {
    let city = ctx.city.clone().unwrap_or("Bangalore".to_string());
    let html = format!(
        r##"
<!DOCTYPE html>
<html>
  <head>
    <script src="/static/htmx.js"></script>
    <script src="/static/tailwind.3.4.3.css.js"></script>
    <script>
    localStorage.theme = 'light';
    </script>
  </head>
  <body>
    <h1 class="m-8 flex justify-center text-2xl sm:text-3xl font-bold text-slate-900 tracking-tight">Superposition Demo App</h1>
    <div id="content" class="mx-20 my-14 content-stretch grid grid-cols-1">
      <form id="context-form" hx-get="/fragments/app.html" hx-target="#fare-table" hx-trigger="change">
        <label class="block mb-2 text-lg font-medium text-gray-900 dark:text-white">Select a city</label>
        <select class="bg-gray-50 text-lg border border-gray-300 text-gray-900 rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500" name="city" >
          <option value="Bangalore" selected>Bangalore</option>
          <option value="Chennai">Chennai</option>
          <option value="Seattle">Seattle</option>
        </select>
        <label class="mt-4 block mb-2 text-lg font-medium text-gray-900 dark:text-white">Select vehicle type</label>
        <select class="bg-gray-50 text-lg border border-gray-300 text-gray-900 rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500" name="vehicle_type">
          <option value="cab" selected>Cab</option>
          <option value="auto">Auto</option>
        </select>

        <label class="mt-4 block mb-2 text-lg font-medium text-gray-900 dark:text-white">Select hour of day</label>
        <select class="bg-gray-50 text-lg border border-gray-300 text-gray-900 rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500" name="hour_of_day">
          <option value="0" selected>0</option>
          <option value="1">1</option>
          <option value="2">2</option>
          <option value="3">3</option>
          <option value="4">4</option>
          <option value="5">5</option>
          <option value="6">6</option>
          <option value="7">7</option>
          <option value="8">8</option>
          <option value="9">9</option>
          <option value="10">10</option>
          <option value="11">11</option>
          <option value="12">12</option>
          <option value="13">13</option>
          <option value="14">14</option>
          <option value="15">15</option>
          <option value="16">16</option>
          <option value="17">17</option>
          <option value="18">18</option>
          <option value="19">19</option>
          <option value="20">20</option>
          <option value="21">21</option>
          <option value="22">22</option>
          <option value="23">23</option>
</select>

      <form>
    </div>
    <div class="mx-20 content-stretch grid grid-cols-1" id="fare-table" hx-get="/fragments/app.html?city={city}" hx-trigger="load">
    </div>
  </body>
</html>"##
    );

    HttpResponse::Ok().body(html)
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new().service(app).service(home).service(
            fs::Files::new("/static", "./examples/superposition-demo-app/web/")
                .show_files_listing()
                .index_file("index.html"),
        )
    })
    .bind(("0.0.0.0", 9090))?
    .run()
    .await
}
