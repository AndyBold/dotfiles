#!/usr/local/bin/python3

# A script that takes an input date and runs various calculations based on that date.
#
# Things to know:
# - First day of the week is Monday.
# - Last day of the week is Sunday.
# - Last day of the month will be the 28th, 30th, or 31st of whatever month the passed in date sits in
#
# Requirements:
#   Python 3 (I use Homebrew on MacOS to get this)
#   python-dateutil
#
# Setting up:
#   brew install python
#   pip3 install python-dateutil


import sys
from optparse import OptionParser
from dateutil.relativedelta import *
from dateutil.rrule import *
from dateutil.parser import *
from dateutil.tz import *
from datetime import *
import os

def main():
  usage = "usage: %prog [options] [DateString]\n\n\tNote: Weeks start on Monday and end on Sunday."

  parser = OptionParser(usage=usage)

  parser.add_option("--ldow", action="store_const", const="ldow", dest="action", help="Return last day of week for the specified date.")
  parser.add_option("--ldom", action="store_const", const="ldom", dest="action", help="Return last day of month for the specified date.")
  parser.add_option("--wse", action="store_const", const="wse", dest="action", help="Returns the start and end dates for the week containing the specified date.")
  parser.add_option("--lmse", action="store_const", const="lmse", dest="action", help="Returns the start and end dates for last month.")

  parser.add_option("--lwmon", action="store_const", const="lwmon", dest="action", help="Returns the date of Monday last week.")
  parser.add_option("--lwtue", action="store_const", const="lwtue", dest="action", help="Returns the date of Tuesday last week.")
  parser.add_option("--lwwed", action="store_const", const="lwwed", dest="action", help="Returns the date of Wednesday last week.")
  parser.add_option("--lwthu", action="store_const", const="lwthu", dest="action", help="Returns the date of Thursday last week.")
  parser.add_option("--lwfri", action="store_const", const="lwfri", dest="action", help="Returns the date of Friday last week.")
  parser.add_option("--lwsat", action="store_const", const="lwsat", dest="action", help="Returns the date of Saturday last week.")
  parser.add_option("--lwsun", action="store_const", const="lwsun", dest="action", help="Returns the date of Sunday last week.")

  parser.add_option("--twmon", action="store_const", const="twmon", dest="action", help="Returns the date of Monday this week.")
  parser.add_option("--twtue", action="store_const", const="twtue", dest="action", help="Returns the date of Tuesday this week.")
  parser.add_option("--twwed", action="store_const", const="twwed", dest="action", help="Returns the date of Wednesday this week.")
  parser.add_option("--twthu", action="store_const", const="twthu", dest="action", help="Returns the date of Thursday this week.")
  parser.add_option("--twfri", action="store_const", const="twfri", dest="action", help="Returns the date of Friday this week.")
  parser.add_option("--twsat", action="store_const", const="twsat", dest="action", help="Returns the date of Saturday this week.")
  parser.add_option("--twsun", action="store_const", const="twsun", dest="action", help="Returns the date of Sunday this week.")

  parser.add_option("--nwmon", action="store_const", const="nwmon", dest="action", help="Returns the date of Monday next week.")
  parser.add_option("--nwtue", action="store_const", const="nwtue", dest="action", help="Returns the date of Tuesday next week.")
  parser.add_option("--nwwed", action="store_const", const="nwwed", dest="action", help="Returns the date of Wednesday next week.")
  parser.add_option("--nwthu", action="store_const", const="nwthu", dest="action", help="Returns the date of Thursday next week.")
  parser.add_option("--nwfri", action="store_const", const="nwfri", dest="action", help="Returns the date of Friday next week.")
  parser.add_option("--nwsat", action="store_const", const="nwsat", dest="action", help="Returns the date of Saturday next week.")
  parser.add_option("--nwsun", action="store_const", const="nwsun", dest="action", help="Returns the date of Sunday next week.")

  (options, args) = parser.parse_args()

  # Set a default date of today.
  argDate = datetime.now().strftime("%d/%m/%Y")

  # We need at least one option
  if not options.action:
    parser.error("No options specified.")

  # We only expect one arg - the date to be parsed.
  if len(args) > 1:
    parser.error("ERROR: Too many arguments passed.\n")

  elif len(args) == 1:
    argDate = args[0]
  
  else:
    # If we don't have any args then we must be using a 'lw' or 'tw' function. Exit with a
    # message if we have been passed an option that requires a date.
    if options.action[:2] not in ["tw", "lw", "nw", "lm"]:
      parser.error("ERROR: Date is required for option", options.action)

  # Convert the date
  myDate = parse(argDate, dayfirst=True)
  # Debug line
  # print("Date:", myDate.date())

  # Our function names are the same as our option names,
  # so we can call the functions using the option name...
  result = globals()[options.action](myDate)
  print(f"{result}")

def ldow(calcDate):
  '''Return the date of the last day in the week'''
  return((calcDate+relativedelta(weekday=SU)).date())

def ldom(calcDate):
  '''Return the date of the last day in the month'''
  return(((calcDate+relativedelta(months=+1, day=1))+relativedelta(days=-1)).date())

def wse(calcDate):
  '''Return the start and end dates of the week containing the specified date.'''
  startDate = (calcDate+relativedelta(weekday=MO(-1))).date()
  endDate = (calcDate+relativedelta(weekday=SU)).date()
  return f"{startDate},{endDate}"

def lmse(calcDate):
  '''Return the start and end dates for last month.'''
  startDate = (calcDate+relativedelta(months=-1, day=1)).date()
  endDate = ldom((calcDate+relativedelta(months=-1, day=1)))
  # return(startDate.strftime("%Y-%m-%d"),endDate.strftime("%Y-%m-%d"))
  return f"{startDate},{endDate}"
  # return(startDate,endDate)


# There is a lot of repetition in the functions below. I could probably
# be more pythonic about this and do something Class-y. But if I did I would
# still need just as many functions, to deal with day names for example, and a
# Class or additional general purpose function. Additional Complexity Is Bad.

##############################################
# Functions that get dates for Last Week (lw)
##############################################
def lwmon(calcDate):
  '''Return the date for last Monday'''
  return((calcDate+relativedelta(weeks=-1, weekday=MO)).date())

def lwtue(calcDate):
  '''Return the date for last Tuesday'''
  return((calcDate+relativedelta(weeks=-1, weekday=TU)).date())

def lwwed(calcDate):
  '''Return the date for last Wednesday'''
  return((calcDate+relativedelta(weeks=-1, weekday=WE)).date())

def lwthu(calcDate):
  '''Return the date for last Thursday'''
  return((calcDate+relativedelta(weeks=-1, weekday=TH)).date())

def lwfri(calcDate):
  '''Return the date for last Friday'''
  return((calcDate+relativedelta(weeks=-1, weekday=FR)).date())

def lwsat(calcDate):
  '''Return the date for last Saturday'''
  return((calcDate+relativedelta(weeks=-1, weekday=SA)).date())

def lwsun(calcDate):
  '''Return the date for last Sunday'''
  return((calcDate+relativedelta(weeks=-1, weekday=SU)).date())


##############################################
# Functions that get dates for This Week (tw)
##############################################
def twmon(calcDate):
  '''Return the date for Monday this week'''
  return((calcDate+relativedelta(weekday=MO)).date())

def twtue(calcDate):
  '''Return the date for Tuesday this week'''
  return((calcDate+relativedelta(weekday=TU)).date())

def twwed(calcDate):
  '''Return the date for Wednesday this week'''
  return((calcDate+relativedelta(weekday=WE)).date())

def twthu(calcDate):
  '''Return the date for Thursday this week'''
  return((calcDate+relativedelta(weekday=TH)).date())

def twfri(calcDate):
  '''Return the date for Friday this week'''
  return((calcDate+relativedelta(weekday=FR)).date())

def twsat(calcDate):
  '''Return the date for Saturday this week'''
  return((calcDate+relativedelta(weekday=SA)).date())

def twsun(calcDate):
  '''Return the date for Sunday this week'''
  return((calcDate+relativedelta(weekday=SU)).date())


##############################################
# Functions that get dates for Next Week (nw)
##############################################
def nwmon(calcDate):
  '''Return the date for next Monday'''
  return((calcDate+relativedelta(weeks=+1, weekday=MO)).date())

def nwtue(calcDate):
  '''Return the date for next Tuesday'''
  return((calcDate+relativedelta(weeks=+1, weekday=TU)).date())

def nwwed(calcDate):
  '''Return the date for next Wednesday'''
  return((calcDate+relativedelta(weeks=+1, weekday=WE)).date())

def nwthu(calcDate):
  '''Return the date for next Thursday'''
  return((calcDate+relativedelta(weeks=+1, weekday=TH)).date())

def nwfri(calcDate):
  '''Return the date for next Friday'''
  return((calcDate+relativedelta(weeks=+1, weekday=FR)).date())

def nwsat(calcDate):
  '''Return the date for next Saturday'''
  return((calcDate+relativedelta(weeks=+1, weekday=SA)).date())

def nwsun(calcDate):
  '''Return the date for next Sunday'''
  return((calcDate+relativedelta(weeks=+1, weekday=SU)).date())


if __name__ == "__main__":
  main()
