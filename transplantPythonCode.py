from datetime import datetime
from typing import List, Any
import csv


def assignDaysAlive():
    print("placeholder")


def printRawData(recorded_data:List):
    for row in recorded_data:
        print(row)


def printTableData(recorded_data:List):
    for row in recorded_data:
        print()
        for item in row:
            print(f"{item:<19}", end="")


def convertToCsv(recorded_data: List):
    with open('finished_data.csv', 'w', newline='') as f:
        csv_writer = csv.writer(f)
        csv_writer.writerows(recorded_data)


def main():
    recorded_data: List[List[any]]
    with open('patientCodebook-42249-ajk-confidential.csv', 'r', newline='')as file:
        mrn_data = csv.reader(file)
        mrn_list = list(mrn_data)
    with open('demographics-42249-ajk-confidential.csv', 'r', newline='') as file:
        info_data = csv.reader(file)
        info_list = list(info_data)
    recorded_data = [[] for _ in info_list]
    for i in range(len(info_list)):
        recorded_data[i].extend([info_list[i][0], mrn_list[i][2], info_list[i][26]])

    with open('new_redcap_data.csv', 'r', newline='') as file:
        redcap_data = csv.reader(file)
        redcap_list = list(redcap_data)
    for row in recorded_data:
        for i in redcap_list:
            if len(row) > 0 and i[3] == row[1]: #csv files with MRNs need to keep leading zeroes
                row.append(i[138])
                break # not necessary but have rn cause error
        if len(row) != 4:
            row.append("1/1/2001") #in case of mismatch MRNs

    recorded_data[0].append("Days Alive")
    for row in recorded_data:
        if row[2] == "FALSE":
            row.append(365)
        else:
            recorded_status = False
            for line in redcap_list:
                if row[1] == line[3] and row[1] != "MRN" and not recorded_status:
                    death_date = line[181]
                    recorded_status = True
                    if death_date == "": # missing deathdate b/c mismatch alive status
                        row.append(-1)
                    else:
                        date_format = "%m/%d/%Y"
                        a = datetime.strptime(row[3], date_format)
                        b = datetime.strptime(death_date, date_format)
                        delta = b - a
                        if delta.days < 365:
                            row.append(delta.days)
                        else:
                            row.append(365)

    recorded_data[0].append("Hospital Days")
    for i in range(1, len(recorded_data)):  # list hospital days as 0 for default
        recorded_data[i].append(0)

    with open('encounters-42249-ajk-confidential.csv', 'r', newline='') as file:
        encounters_data = csv.reader(file)
        encounters_list = list(encounters_data)

    for i in range(len(encounters_list)):
        if encounters_list[i][3] == "Admission (Discharged)":
            patient_id = int(encounters_list[i][0])  # temp
            raw_admission_date = encounters_list[i][7]
            admission_date = (raw_admission_date.split(' '))[0]
            raw_discharge_date = encounters_list[i][9]
            discharge_date = (raw_discharge_date.split(' '))[0]
            #holy fucking mess
            if admission_date == "": #missing admission date
                pass
            else:
                datetime_format = "%m/%d/%Y %H:%M"
                datetime1 = datetime.strptime(raw_admission_date, datetime_format)
                datetime2 = datetime.strptime(raw_discharge_date, datetime_format)
                time_difference = (datetime2 - datetime1).total_seconds() / 3600
                if time_difference < 24:
                    pass
                else:
                    transplant_date = recorded_data[patient_id][3]
                    date_format = "%m/%d/%Y"
                    a = datetime.strptime(transplant_date, date_format)
                    b = datetime.strptime(admission_date, date_format)
                    delta = b - a
                    aDistance = delta.days
                    a = datetime.strptime(transplant_date, date_format)
                    b = datetime.strptime(discharge_date, date_format)
                    delta = b - a
                    dDistance = delta.days
                    #print("Patient ID: ", patient_id)  # temp
                    #print(admission_date, " + ", discharge_date, " + ", transplant_date)
                    #print(aDistance, " + ", dDistance)
                    if aDistance < 0 and dDistance < 0:
                        print(end="")
                    elif aDistance > recorded_data[patient_id][4] and dDistance > recorded_data[patient_id][4]:
                        print(end="")
                    elif aDistance < 0 and dDistance <= recorded_data[patient_id][4]:
                        recorded_data[patient_id][5] += dDistance
                    elif aDistance >= 0 and dDistance <= recorded_data[patient_id][4]:
                        a = datetime.strptime(admission_date, date_format)
                        b = datetime.strptime(discharge_date, date_format)
                        delta = b - a
                        recorded_data[patient_id][5] += delta.days
                    elif aDistance >= 0 and dDistance > recorded_data[patient_id][4]:
                        recorded_data[patient_id][5] += recorded_data[patient_id][4] - aDistance

    recorded_data[0].append("DAOH")
    for i in range(1, len(recorded_data)):
        daoh = recorded_data[i][4] - recorded_data[i][5]
        recorded_data[i].append(daoh)

    printTableData(recorded_data)
    convertToCsv(recorded_data)


if __name__ == "__main__":
    main()

""""
I feel like the code would be. .  .maybe 3x better if instead of appending, you change
the coordinate value. this ensures no 'out of index range' errors emerge
but lowkey not necessary right now

notes:
emergency admissions [DONE] by DEFAULT
24 hour metric vs. overnight for daoh [DONE]
code around "previous transplant dates" [DONE]

there still seems to be some calculation errors?! left off with scrutinizing 613

maybe next goal is to automate the process so a larger variety of files can go and still come out

SUB-ANALYSIS; on the condition u live (exclude everyone who has died)
x+y=z
glm = generalized linear model
code in r?
"""
