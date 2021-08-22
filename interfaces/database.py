import sqlite3

from sqlite3 import Error as sqlError

def read_database():

    self.conn = None

    try:
        self.conn = sqlite3.connect(self.filepath)
    except Error:
        print(Error)
    return

def write_database():

    # _set_database_table(self,sqlite_table):

    # instructor_table = """ CREATE TABLE IF NOT EXISTS instructors (
    #                                     id integer PRIMARY KEY,
    #                                     first_name text NOT NULL,
    #                                     last_name text NOT NULL,
    #                                     patronym text NOT NULL,
    #                                     position text NOT NULL,
    #                                     status text NOT NULL,
    #                                     email text NOT NULL UNIQUE,
    #                                     phone integer NOT NULL UNIQUE);"""

    self.sqlite_table = sqlite_table

    try:
        self.cursor = self.conn.cursor()
        self.cursor.execute(self.sqlite_table)
        self.conn.commit()
    except Error:
        print(Error)

    # _insert_database_table(self,sqlite_table_insert,table_row):

    # instructor_table_insert = """ INSERT INTO instructors(
    #                                     id,
    #                                     first_name,
    #                                     last_name,
    #                                     patronym,
    #                                     position,
    #                                     status,
    #                                     email,
    #                                     phone)
    #                                     VALUES(?,?,?,?,?,?,?,?)"""

    self.sqlite_table_insert = sqlite_table_insert
    self.cursor.execute(self.sqlite_table_insert,table_row)
    self.conn.commit()

if __name__ == "__main__":

    dbpath = r"C:\Users\Cavid\Documents\bhospy\interfaces\instructors.db"
    
    DB = database_manager(dbpath)

    DB.create_table(instructor_table)

    instructor = (7,"Javid","Shiriyev","Farhad",
                "Senior Lecturer","Hour Based Teaching",
                "cavid.shiriyev@bhos.edu.az","+994508353992")

    DB.insert_table(instructor_table_insert,instructor)
    DB.cursor.close()
    DB.conn.close()
