# apis: R clients for telemetry APIs

This package provides clients for the Argos, Ecotopia/DRUID, and Kinéis APIs.
It handles authentication, device discovery, and retrieval of locations,
sensor values, behaviour, and other telemetry.

## Installation

```r
remotes::install_github("ornitho-logics/apis")
```
## Argos API

1. `argos_login(un, pwd, wsdl_server)`: Authenticate.
2. `argos_devlist(login)`: Retrieve a list of devices.
3. `argos_data(login, platformId, startDate)`: Retrieve one platform and time
   window.

Argos returns the requested platform window as one SOAP/CSV response; it does
not use the cursor loop used by Ecotopia and Kinéis.

```text
authenticate and select WSDL endpoint
    ↓
retrieve platform list
    ↓
request one platform and time window
    ↓
extract CSV from the SOAP response
    ↓
return a data.table
```

## Ecotopia/DRUID API

1. `ecotopia_login(un, pwd, kw1, kw2)`: Authenticate.
2. `ecotopia_devlist(logstring)`: Retrieve a list of devices.
3. `ecotopia_data(logstring, id, datetime, what)`: Retrieve one data type for a
   device.
4. `ecotopia_postprocess_structured(behavior)`: Decode structured behaviour
   payloads.

Device discovery uses a device-ID cursor. Telemetry uses the latest timestamp
from each page as the next cursor.

```text
authenticate
    ↓
request device or telemetry page
    ↓
append the returned records
    ↓
select the next device-ID or timestamp cursor
    ↓
request next page
    ↺ until the page is empty or the end time is reached
    ↓
deduplicate by API record ID
    ↓
return a data.table
```

## Kinéis API

1. `kineis_login(un, pwd, auth_url)`: Obtain a JWT.
2. `kineis_devlist(token, api_telemetry_url)`: Retrieve the device list.
3. `kineis_data(token, api_telemetry_url, datetime, ...)`: Retrieve bulk
   telemetry pages, optionally resuming from a saved cursor.
4. `kineis_data_count(token, api_telemetry_url, datetime, ...)`: Count
   account-wide telemetry messages to size historical retrieval windows.

Kinéis bulk retrieval uses `pageInfo$endCursor`. Pages are requested in
ascending `msgDatetime` order. Requests are paced, HTTP 429/503 responses are
retried, and a token-provider function can renew an expired JWT after HTTP 401.

```text
obtain or renew JWT
    ↓
request bulk page
    ↓
return page to page_handler, when supplied
    ↓
read pageInfo$endCursor
    ↓
request next cursor
    ↺ while pageInfo$hasNextPage is true
    ↓
combine pages when collect = TRUE
    ↓
return a data.table
```

The `apis` package only retrieves and transforms API responses. Database
persistence is implemented by the `dup` package.

## License

MIT License.

## Contributing

Feel free to open issues or submit pull requests for bug fixes or enhancements.
