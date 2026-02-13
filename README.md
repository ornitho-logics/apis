
# apis: R Package for Interacting with Argos and Ecotopia APIs

This package provides functions to interact with the Argos and DRUID Ecotopia APIs, allowing users to authenticate, retrieve device information, and access platform data, such as GPS locations, sensor readings, and more.

## Installation

You can install the `apis` package directly from GitHub:

```r
remotes::install_github("ornitho-logics/apis")
```

## Main Functions

### Argos API

1. `argos_login(un, pwd, wsdl_server)`: Authenticate.

2. `argos_devlist(login)`: Retrieve a list of devices.

3. `argos_data(login, platformId, nbDaysFromNow)`: Download data.

### Ecotopia API

1. `ecotopia_login(un, pwd, kw1, kw2)`: Authenticate.

2. `ecotopia_devlist(logstring)`: Retrieve a list of devices.

3. `ecotopia_data(logstring, id, datetime, what)`: Download data.


## License
MIT License.

## Contributing
Feel free to open issues or submit pull requests for bug fixes or enhancements.
