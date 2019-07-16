// Copyright (C) Ben Lewis.
// Licensed under the MIT License, see LICENSE.

open Newtonsoft.Json.Linq
open System
open System.Linq
open System.Net.Http

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

let expandValue (value : JToken) =
    (value.Value<string>("validTime"), value.Value<string>("value"))

let parseForecastResponse (forecast : JObject) =
    // We have some set of days and times. Start with daily weather, and add
    // hourly as we go.
    // Daily forecast!
    let minimumTemps = Seq.map expandValue (forecast.SelectTokens("$.minTemperature.values[*]"))
    let maximumTemps = Seq.map expandValue (forecast.SelectTokens("$.maxTemperature.values[*]"))
    // weather is a rich object, with a value that's a bit more significant.
    Seq.iter2 (fun (timeMin, minTemp) (timeMax, maxTemp) -> (printfn "%s: Low: %s High: %s" timeMin minTemp maxTemp)) minimumTemps maximumTemps
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

[<EntryPoint>]
let main argv =
    printfn "NWS Client"
    
    let httpClient = new HttpClient ()
    httpClient.BaseAddress <- Uri ("https://api.weather.gov")
    httpClient.DefaultRequestHeaders.Accept.Add (Headers.MediaTypeWithQualityHeaderValue ("application/ld+json"))
    httpClient.DefaultRequestHeaders.UserAgent.Add (Headers.ProductInfoHeaderValue ("zen-nws-client", "0.1"))
    httpClient.DefaultRequestHeaders.UserAgent.Add (Headers.ProductInfoHeaderValue ("(benjf5+nws@gmail.com)"))
    printfn "Getting basic data for Redmond City Hall:"
    let pointText = makeRequest httpClient "./points/47.678878,-122.130496" |> Async.RunSynchronously
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

    let forecastOption 
        = Option.map (makeRequest httpClient) gridpointEndpoint 
            |> Option.map Async.RunSynchronously
            |> Option.map JObject.Parse
    match forecastOption with
    | Some(weatherForecast)  ->
        parseForecastResponse weatherForecast
    // printfn "%s" weatherForecast 
    | None -> printfn "No weather forecast retrieved"

    0 // return an integer exit code
