// Copyright (C) Ben Lewis.
// Licensed under the MIT License, see LICENSE.

open Newtonsoft.Json.Linq
open System
open System.IO
open System.Linq
open System.Net.Http
open System.Xml

let selectToken (obj : JObject) path =
    match obj.SelectToken path with
    | null -> None
    | token -> Some(token)

exception RequestException of string * int
let makeRequest (httpClient : HttpClient) (requestUri : string) =
    async {
        let! response = httpClient.GetAsync requestUri |> Async.AwaitTask
        if not response.IsSuccessStatusCode then
            raise (RequestException(requestUri, int response.StatusCode))
            return ""
        else
            let! responseContent = response.Content.ReadAsStringAsync () |> Async.AwaitTask
            return responseContent
    }

type ForecastDuration = {time: DateTime; duration: TimeSpan}

type ForecastEntity<'T> = { duration: ForecastDuration; name: string; value: 'T }

let parse_forecast_time (time_string : string) = 
  time_string.Split("/") |> Array.toList |>
    fun x -> match x with
             | t :: [d] -> { time = DateTime.Parse t; duration = XmlConvert.ToTimeSpan d  }
             | _ -> raise (ArgumentException ("Unexpected forecast time string provided: " + time_string))

let expandValue name parse_fn (value : JToken) =
    { name = name;
      duration = (parse_forecast_time (value.Value<string>("validTime")));
      value = (parse_fn (value.Value<string>("value"))) }

let parseForecastResponse (forecast : JObject) =
    // We have some set of days and times. Start with daily weather, and add
    // hourly as we go.
    forecast.Properties () |> Seq.map (fun x -> (string) x) |> Seq.iter (printfn "Property: %s")
    // Daily forecast!
    let minimumTemps = forecast.SelectTokens("$.minTemperature.values[*]") |> Seq.map (expandValue "Low" Double.Parse)
    let maximumTemps = forecast.SelectTokens("$.maxTemperature.values[*]") |> Seq.map (expandValue "High" Double.Parse)
    // weather is a rich object, with a value that's a bit more significant.
    Seq.iter2 (fun lowTemp highTemp -> (printfn "%O, %O" lowTemp highTemp)) minimumTemps maximumTemps
    // Hourly forecast:
    // temperature
    // dewpoint
    // relativeHumidity
    // apparentTemperature
    // heatIndex
    // windChill (misses data sometimes)
    // skyCover (6 hours)
    // windDirection (uneven hours)
    // windSpeed
    // windGust
    //

type Argument = 
  | Verbose
  | Test of string
  | OutFile of string

let rec parse_arguments_internal (args : string list) results =
  match args with
  | [] -> results
  | arg :: more ->
    match arg.ToLowerInvariant() with
    | "-verbose" ->  parse_arguments_internal more (Verbose :: results)
    | "-test" ->
      match more with
      | [] -> raise (ArgumentException "-test must be followed by a filename")
      | file :: addl -> parse_arguments_internal addl (Test(file) :: results)
    | "-out" ->
      match more with
      | [] -> raise (ArgumentException "-out must be followed by a filename")
      | file :: addl -> parse_arguments_internal addl (OutFile(file) :: results)
    | _ -> raise (ArgumentException ("Unrecognized parameter supplied: " + arg))

let parse_arguments argv =
  parse_arguments_internal argv []

let argument_path arg =
  match arg with
  | Verbose -> None
  | Test(test_path) -> Some(test_path)
  | OutFile(out_path) -> Some(out_path)



let load_data test_option =
  match test_option with
  | Some(Test(test_file)) ->
    printfn "Loading JSON data file from %s" test_file
    Some(File.ReadAllText(test_file))
  | Some(_) -> None
  | None ->
    let httpClient = new HttpClient ()
    httpClient.BaseAddress <- Uri ("https://api.weather.gov")
    httpClient.DefaultRequestHeaders.Accept.Add (Headers.MediaTypeWithQualityHeaderValue ("application/ld+json"))
    httpClient.DefaultRequestHeaders.UserAgent.Add (Headers.ProductInfoHeaderValue ("zen-nws-client", "0.1"))
    httpClient.DefaultRequestHeaders.UserAgent.Add (Headers.ProductInfoHeaderValue ("(benjf5+nws@gmail.com)"))
    printfn "Getting basic data for Redmond City Hall:"
    let pointText =
      makeRequest httpClient "./points/47.678878,-122.130496"
      |> Async.RunSynchronously
    let pointData = JObject.Parse pointText
    let officeName = Option.map string (selectToken pointData "$.cwa")
    // get the grid point for the office
    let gridX = Option.map string (selectToken pointData "$.gridX")
    let gridY = Option.map string (selectToken pointData "$.gridY")
    // build the URI to request
    let gridpointEndpoint = Option.map3 (sprintf "./gridpoints/%s/%s,%s") officeName gridX gridY
    match gridpointEndpoint with
    | Some(uri) -> printfn "Making a request to %s for the weather forecast" uri
    | None -> printfn "No Uri could be constructed."

    Option.map (makeRequest httpClient) gridpointEndpoint 
    |> Option.map Async.RunSynchronously

[<EntryPoint>]
let main argv =
    printfn "NWS Client"
    
    let arguments = parse_arguments (List.ofArray argv)
    let testArgument = 
      List.tryFind (fun arg ->
                      match arg with
                      | Test _ -> true
                      | _ -> false) arguments
    
    let data = load_data testArgument

    match (Option.map JObject.Parse data) with
    | Some(weatherForecast) ->
        parseForecastResponse weatherForecast
        let outFile = List.tryFind (fun arg ->
                                      match arg with
                                      | OutFile _ -> true
                                      | _ -> false) arguments
                      |> Option.bind argument_path
        Option.iter
          (fun path ->
            printfn "Writing out retrieved forecast to %s" path
            Option.iter (fun x -> File.WriteAllText (path, x)) data) outFile
    | None -> printfn "No weather forecast retrieved"

    0 // return an integer exit code
