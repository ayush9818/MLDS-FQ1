import numpy as np
import pandas as pd
import os
import sqlite3

"""
Populates a db according to specifications for HW5 - Northwestern MSiA 413 Course taught by Nikos Hardavellas

The db file returned will be located in the current directory as  "hw5-pythonOutput.db".
You need to have the file 'hw5_original.csv' in the current directory  for this to run. 
To change the name of the file edit the input to the main() function

Author: Jide Anene, modified by Nikos Hardavellas
Email: jideanene2020@u.northwestern.edu
Date: 12/11/2019

python 3.8

"""

def create_connection(db_file):
        """ 
        Create a database connection to the SQLite database specified by db_file
        :param db_file: database file
        :return: Connection object or None
        """
        conn = None
        try:
                conn = sqlite3.connect(db_file)
                ######conn.text_factory = str
                return conn
        except Error as e:
                print(e)
 
        return conn


def execute_sql_op(conn, sql_op):
        """ 
        Executes a sql statement 
        :param conn: Connection object
        :param sql_op: a SQL statement
        :return:
        """
        try:
                c = conn.cursor()
                c.execute(sql_op)
        except Error as e:
                print(e)

def main(dbName):


        operations = dict()

        operations["op_0"] = """drop table if exists invoice_items;"""
        operations["op_1"] = """drop table if exists tracks;"""
        operations["op_2"] = """drop table if exists invoices;"""
        operations["op_3"] = """drop table if exists customers;"""
        operations["op_4"] = """drop table if exists albums;"""
        operations["op_5"] = """drop table if exists artists;"""
        operations["op_6"] = """drop table if exists genres;"""
        operations["op_7"] = """drop table if exists media_types;"""

        operations["op_8"] = """create table media_types (
                                        mediaTypeId integer not null primary key autoincrement,
                                        mediaName nvarchar(20) not null
                                );"""
        operations["op_9"] = """insert into media_types (mediaName)
                                        select distinct mediaType from hw5_original;"""

        operations["op_10"] = """create table genres (
                                        genreId integer not null primary key autoincrement,
                                        genreName nvarchar(20) not null
                                );"""
        operations["op_11"] = """insert into genres (genreName)
                                        select distinct genre from hw5_original;"""

        operations["op_12"] = """create table artists (
                                        artistId integer not null primary key autoincrement,
                                        artistName text not null,
                                        unique(artistName)
                                );"""
        operations["op_13"] = """insert into artists (artistName)
                                        select distinct artistName from hw5_original;"""

        operations["op_14"] = """create table albums (
                                        albumId integer not null primary key autoincrement,
                                        albumTitle nvarchar(200) not null,
                                        artistId integer not null references artists(artistid)
                                );"""
        operations["op_15"] = """insert into albums (albumTitle, artistId)
                                        select distinct albumTitle, artistId
                                        from hw5_original
                                                join artists on artists.artistName = hw5_original.artistName;"""

        operations["op_16"] = """create table customers (
                                        customerId integer not null primary key autoincrement,
                                        firstName nvarchar(200) not null,
                                        lastName nvarchar(200) not null,
                                        address nvarchar(300),
                                        city nvarchar(100),
                                        state nvarchar(10),
                                        country nvarchar(20),
                                        postalCode nvarchar(20),
                                        phoneNumber nvarchar(30),
                                        faxNumber nvarchar(30),
                                        email nvarchar(80) not null,
                                        unique(lastName)
                                );"""
        operations["op_17"] = """insert into customers
                                                (lastName, firstName,
                                                address, city, state,
                                                country, postalCode,
                                                phoneNumber, faxNumber, email)
                                        select distinct customerLastName, customerFirstName,
                                                customerAddress, customerCity, CustomerState,
                                                CustomerCountry, CustomerPostalCode,
                                                CustomerPhone, CustomerFax, CustomerEmail
                                        from hw5_original
                                        where customerLastName is not null;"""

        operations["op_18"] = """create table invoices (
                                        invoiceId integer not null primary key autoincrement,
                                        Date datetime not null,
                                        billingAddress nvarchar(300),
                                        billingCity nvarchar(100),
                                        billingState nvarchar(10),
                                        billingCountry nvarchar(20),
                                        billingPostalCode nvarchar(20),
                                        customerId integer not null references customers(customerId),
                                        originalInvoiceId integer
                                );"""
        operations["op_19"] = """insert into invoices (originalInvoiceId, Date,
                                                billingAddress, billingCity, billingState,
                                                billingCountry, billingPostalCode, customerId)
                                        select distinct invoiceId, invoiceDate,
                                                invoiceBillingAddress, invoiceBillingCity, invoiceBillingstate,
                                                invoiceBillingCountry, invoiceBillingPostalCode, customerId
                                        from hw5_original
                                                join customers on customers.firstName = customerFirstName
                                                                and customers.lastName = customerLastName;"""

        operations["op_20"] = """create table tracks (
                                        trackId integer not null primary key autoincrement,
                                        trackName nvarchar(200) not null,
                                        composer nvarchar(200),
                                        trackSizeByte integer,
                                        tracklength integer not null,
                                        trackprice numeric not null,
                                        genreId integer not null references genres(genreId),
                                        mediaTypeId integer not null references media_types(mediaTypeId),
                                        albumId integer not null references albums(albumId),
                                        unique(trackName,trackLength)
                                );"""
        operations["op_21"] = """insert into tracks (trackName, composer, trackSizeByte, tracklength, trackprice,
                                                        genreid, mediaTypeId, albumId)
                                        select distinct trackName, Composer, TrackSizeBytes, TrackLength, TrackPrice,
                                                genreId, mediaTypeId, albumId
                                        from hw5_original join genres on genres.genreName = hw5_original.Genre
                                                join albums on albums.albumTitle = hw5_original.AlbumTitle
                                                join media_types on media_types.mediaName = hw5_original.MediaType;"""

        operations["op_22"] = """create table invoice_items (
                                        invoiceItemId integer not null primary key autoincrement,
                                        invoiceId integer not null references invoices(invoiceId),
                                        trackId integer not null references tracks(trackId),
                                        quantity integer not null,
                                        unitPrice numeric not null
                                );"""
        operations["op_23"] = """insert into invoice_items (invoiceid, trackid, quantity, unitPrice)
                                        select invoices.invoiceId, tracks.trackid, invoiceItemQuantity, invoiceItemUnitPrice
                                        from hw5_original
                                                join tracks on tracks.trackName = hw5_original.TrackName
                                                                and tracks.trackLength = hw5_original.TrackLength
                                                join invoices on invoices.originalInvoiceId = hw5_original.invoiceId;"""


        # create a database connection
        conn = create_connection(os.path.join(".",dbName+".db"))

        # create tables
        if conn is not None:
                with conn:  
                        if (os.path.exists('hw5_original.csv')):


                                # upload the csv
                                df = pd.read_csv ('hw5_original.csv')
                                df.to_sql(r'hw5_original', conn, if_exists='append', index = False)

                                # create projects table
                                for opId,opVal in operations.items():
                                    execute_sql_op(conn, opVal)

                                # The DB-API spec requires that connecting to the database
                                # begins a new transaction, by default. You must commit to
                                # confirm any changes you make, or rollback to discard them.
                                conn.commit()
                                print("Succesful!")
                        else:
                                print("Error! required csv file 'hw5_original.csv' missing.")
        else:
                print("Error! cannot create the database connection.")


if __name__ == '__main__':
        main("hw5-pythonOutput")
