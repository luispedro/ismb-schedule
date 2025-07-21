import requests
import pandas as pd
from os import path

COLS = [
 'Track',
 'Room',
 'Weekday',
 'Date',
 'Timespan',
 'Format',
 'Speaker',
 'Title',
 'Abstract']

URL = 'https://www.iscb.org/images/stories/ismb2024/document.ScheduleByTrack.ISMB.2024.xlsx'
if not path.exists(path.basename(URL)):
    print('Downloading file...')
    r = requests.get(URL)
    with open(path.basename(URL), 'wb') as f:
        f.write(r.content)

timetable = pd.read_excel('ISMBECCB_2025_All_Detailed_Schedule.xlsx')
timetable.columns = timetable.iloc[0]
timetable = timetable.iloc[1:]
timetable = timetable.rename(columns={'Confrimed Presenter': 'Speaker'})
timetable['Room'] = timetable['Room'].fillna('-').astype(str)
timetable['Date'] = pd.to_datetime(timetable['Date'])
timetable['Weekday'] = timetable['Date'].dt.day_name()

assert set(timetable["Start Time"].dropna().astype(str).str.split(':').str[-1]) == {'00'}
assert set(timetable["End Time"].dropna().astype(str).str.split(':').str[-1]) == {'00'}

timetable['Start Time'] = pd.to_datetime(timetable['Start Time'], format='%H:%M:%S')
timetable['End Time'] = pd.to_datetime(timetable['End Time'], format='%H:%M:%S')

timetable['Timespan'] = timetable['Start Time'].dt.strftime('%H:%M') + '-' + timetable['End Time'].dt.strftime('%H:%M')
timetable['Date'] = timetable['Date'].dt.strftime('%d %B')
timetable = timetable[COLS]
timetable = timetable.query('Track != "This Track is Used for Testing"')

KEYNOTE_SPEAKERS = {
        }

for ix in timetable.query('Track == "Distinguished Keynotes"').index:
    assert pd.isna(timetable.loc[ix].Speaker)
    timetable.loc[ix, 'Speaker'] = KEYNOTE_SPEAKERS[timetable.loc[ix].Title]

with open('ISMB_2025_All_sessions.json', 'wt') as out:
    timetable.to_json(
            out,
            orient='records',
            force_ascii=False)
