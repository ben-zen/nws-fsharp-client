// Copyright (C) Ben Lewis.
// Licensed under the MIT License, see LICENSE.

open Newtonsoft.Json.Linq
open System
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
    let gridpointEndpoint = Option.map3 (sprintf "./gridpoints/%s/%s,%s/forecast") officeName gridX gridY
    match gridpointEndpoint with
    | Some(uri) -> printfn "Making a request to %s for the weather forecast" uri
    | None -> printfn "No Uri could be constructed."

    let forecast = Option.map (makeRequest httpClient) gridpointEndpoint |> Option.map Async.RunSynchronously
    match forecast with
    | Some(weatherForecast)  -> 
        printfn "%s" weatherForecast 
    | None -> printfn "No weather forecast retrieved"

    0 // return an integer exit code
