#!/bin/python3

#
# WebServices.py
#
# These routines were downloaded from the AWDN web site.  These routines can retrieve a list
# of active or all stations, retrieve data for a specific time frame for a particular station,
# and get a grid of data for a station.  This grid routine is not used.
#
import requests,orjson
from requests.packages.urllib3.util.retry import Retry
from requests.adapters import HTTPAdapter

class WebServices:
    """
    Web Services
    
    Class to pythonically make calls to web services.
    
    :param bool test: Use the test server instead
    """
    def __init__(self,test=False):
        self.__server = 'https://awdn1.unl.edu/productdata/get?' if test else 'https://awdn2.unl.edu/productdata/get?'
        retry_strategy = Retry(total=5,status_forcelist=[429, 500, 502, 503, 504],method_whitelist=["HEAD", "GET", "OPTIONS"],backoff_factor=1)
        adapter = HTTPAdapter(max_retries=retry_strategy)
        self.__http = requests.Session()
        self.__http.mount("http://", adapter)
        self.__http.mount("https://", adapter)
        self.__http.headers['User-Agent'] = 'AWDN Web Services'
        
    def getList(self,product='scqc1440',network=None):
        """
        List Stations
        
        Generates a list of all stations in the database for the 
        passed product ID. This call does not require any other 
        parameters. 
        
        :param str product: Product ID
        :param str network: Limit Request by Network
        
        :return: JSON containing:
        * Stationid
        * Name
        * Latitude
        * Longitude
        * Elevation
        * Network
        * Startup
        * Closedown
        * Climate Division Name
        * Climate Division Code
        :rtype: dict
        
        Networks ID are:
        * coagmet - Colorado
        * kstate - Kansas
        * iem - Iowa
        * ndawn - North Dakota, Minnesota, Montana
        * nemesonet - Nebraska
        * wacnet - Wyoming
        """
        params = {"list":product,"network":network} if network is not None else {"list":product}
        response = self.__http.get(self.__server, params=params)
        response.raise_for_status()
        try:
#            return response.json()
            return response.text
        except ValueError as err:
            raise Exception(response.text)
    
    def getActive(self,product='scqc1440',network=None):
        """
        Active Stations
        
        Generates a list of active stations in the database for 
        the passed product ID. This call does not require any 
        other parameters. 
        
        :param str product: Product ID
        :param str network: Limit Request by Network
        
        :return: JSON containing:
        * Stationid
        * Name
        * Latitude
        * Longitude
        * Elevation
        * Network
        * Startup
        * Received
        * Climate Division Name
        * Climate Division Code
        :rtype: dict
        
        Networks ID are:
        * coagmet - Colorado
        * kstate - Kansas
        * iem - Iowa
        * ndawn - North Dakota, Minnesota, Montana
        * nemesonet - Nebraska
        * wacnet - Wyoming
        """
        params = {"active":product,"network":network} if network is not None else {"active":product}
        response = self.__http.get(self.__server, params=params)
        response.raise_for_status()
        try:
#            return response.json()
            return response.text
        except ValueError as err:
            raise Exception(response.text)
        
    def getData(self,name,begin,end,product='scqc1440',format="json",units='si',tz='CST',network="all",sensor="all"):
        """
        Data Request
        
        Gets data for the requested parameters.
        
        
        :param str name: The name, state ID or station ID
        :param str productid: The product to read from
        :param str network: The network abbreviation. Optional and if omitted the name parameter is used alone.
        :param str begin: timestamp to start data retrieval on in the form YYYYmmddHH where: Y = year, m = month, d = day, H = hour. The hour can be omitted to get the full day
        :param str end: timestamp to end data retrieval on in the form YYYYmmddHH where: Y = year, m = month, d = day, H = hour. The hour can be omitted to get the full day
        :param str format: Output format. This is optional and defaults to json.
        * json = Javascript Object Notation 
        * geojson = Geographical Javascript Object Notation 
        * csv = Comma Spaced Values
        :param str tz: Timezone abbreviation to grab the data with. This is optional and defaults to Central Standard Time
        :param str sensor: Retrieve data for a specific sensor. This is optional.
        :param str units: Return data in United States (us) or International System (si) units. The default is International System.
        
        :return: Returns data from Web Services and Contains:
        * Name
        * State
        * County
        * Sensors
        * units
        * data
        * Latitude
        * Longitude
        :rtype: dict
        
        Networks ID are:
        * coagmet - Colorado
        * kstate - Kansas
        * iem - Iowa
        * ndawn - North Dakota, Minnesota, Montana
        * nemesonet - Nebraska
        * wacnet - Wyoming
        """
        params = {"name":name,"productid":product,"begin":begin,"end":end,"format":format}
        if units != 'si':
            params['units'] = units
        if tz != 'CST':
            params['tz'] = tz
        if network != 'all':
            params['network'] = network
        if sensor != 'all':
            params['sensor'] = sensor
        
        response = self.__http.get(self.__server, params=params)
        response.raise_for_status()
        try:
#            return response.json()
            return response.text
        except ValueError as err:
            raise Exception(response.text)
        
    def getGrid(self,date,product='scqc1440'):
        """
        Grid Data Request
         
        Gets grid data for the requested parameters.
         
         
        :param str productid: The product to read. This can be hourly (scqc60), daily (scqc1440) or ET (penet)
        :param str date: timestamp to get data retrieval on in the form YYYYmmddHH where: Y = year, m = month, d = day, H = hour. The hour is omitted when getting daily grids. Yesterdays data can be reteived by using a 'y' instead of the timestamp.
         
        :return: Returns gridded data from Web Services in geojson format.
        :rtype: dict
        """
        params = {"grid":product,"date":date}
        
        response = self.__http.get(self.__server, params=params)
        response.raise_for_status()
        try:
#            return response.json()
            return response.text
        except ValueError as err:
            raise Exception(response.text)
