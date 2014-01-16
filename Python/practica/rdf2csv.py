#!/usr/bin/python

import inspect
from HTMLParser import HTMLParser

allrest=[]

class restaurant:

    def add_name(self, name):
        self.name = name

    def add_street(self, street):
        self.street = street

    def add_district(self, district):
        self.district = district

    def add_neighborhood(self, neighborhood):
        self.neighborhood = neighborhood

    def add_postal_code(self, postal):
        self.postal_code = postal

    def add_country(self, country):
        self.country = country

    def add_latitude(self, lat):
        self.latitude = lat

    def add_longitude(self, lon):
        self.longitude = lon

    def add_telf(self, telf):
        self.telf = telf

# creem una subclasse i sobreescribim el metodes del ha(self, )n
class MHTMLParser(HTMLParser):

    crest = restaurant()
    ctag = ""

    def handle_starttag(self, tag, attrs):
        self.ctag = tag
        if tag == 'v:vcard':
            self.crest = restaurant()

    def handle_endtag(self, tag):
        self.ctag = ""
        if tag == 'v:vcard':
            allrest.append(self.crest)

    def handle_data(self, data):
        if self.ctag == 'v:fn':
            self.crest.add_name(data)
        elif self.ctag == 'v:street-address':
            self.crest.add_street(data)
        elif self.ctag == 'xv:district':
            self.crest.add_district(data)
        elif self.ctag == 'xv:neighborhood':
            self.crest.add_neighborhood(data)
        elif self.ctag == 'v:postal-code':
            self.crest.add_postal_code(data)
        elif self.ctag == 'v:country-name':
            self.crest.add_country(data)
        elif self.ctag == 'v:latitude':
            self.crest.add_latitude(data)
        elif self.ctag == 'v:longitude':
            self.crest.add_longitude(data)
        elif self.ctag == 'rdf:value':
            self.crest.add_telf(data)

# Print function because python print is an statement
def print_f(string):
    print string

# open file
f = open('restaurants.rdf', 'rb')
rdfSource = f.read()
f.close()

parser = MHTMLParser()
parser.feed(rdfSource)

header = ['name', 'street', 'district', 'neighborhood', 'postal_code', 'country', 'latitude', 'longitude', 'telf']
print '\t'.join(header)

for r in allrest:

    # Inicialize all results as undef
    results = {}
    for key in header: results[key] = "undef"

    # get information about object
    # and extract defined vars
    for elem in inspect.getmembers(r):
        name = elem[0]
        if not name.startswith('__') and not name.startswith('add_'):
            results[name] = elem[1] # elem[1] value associated to name

    output = ''
    for key in header:
        output += results[key] + '\t'

    print_f(output)
