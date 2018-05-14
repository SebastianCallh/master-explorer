# Development Notes

This markdown contains some scattered notes of problems and eventual solutions I've experienced during development.

## Nix
Wrestling with Nix took quite a while. To get Dante to work the .emacs needed to be properly configured, which is currently hard coded to the project subpath of the reflex project, which provides the enironment and Reflex dependencies. The `default.nix` needs to contains all projects, and so does the `cabal.project`.

## JSaddle
When running the app in jsaddle-warp the program spontaneously crashes with `Error : Unexpected Duplicate. syncCallbacks=True nBatch=35 nExpected=37` (example numbers) which is awful. This turned out to be a bug in Firefox. Running the app in chromium solved the issue.

### Filtering dynamic
Something very odd happened when I tried to filter a list inside a dynamic. Instead of displaying the filtered items it displayed the n first items, where n is the length of the filtered list. It had something to do with the usage of `sample . current` and filtering several items in a frame. It seems that the built in functions to generate DOM elements only gets you so far. I had to resort to creating them manually using `elDynAttr'`, which worked brilliantly.

### The Scraper
The scraper does not parse all course pages successfully so the selection of courses is not complete. However I have decided to continue developing the web application firstly and then fix this issue when time permits.

### Missing URLS
Some courses does not have urls in the course list, such as Avancerad programvarudesign, Maskininlärning, planering och reglering fär autonoma farkoster, and Avancerad programvarudesign.


### Workflow
Using the workflow tools in Reflex has been pretty neat, but it took quite some time to wrap my head around how to pass data between them in a way that worked as intended (everything being events and dynamics is a bit tricky at times). Upon completing the course selection menu it became apparent that slots are not scraped correctly for all courses, making them simply disappear when being selected.
The courses being scraped correctly are: [TAMS22,TAMS24,TAMS32,TAMS39,TANA09,TANA15,TANA21,TAOP33,TAOP61,TATA24,TATA40,TATA41,TATA42,TATA53,TATA54,TATA55,TATA64,TATA65,TATA76,TATA79,TBME03,TBME04,TBME08,TBMI01,TBMI02,TBMI03,TBMI19,TBMI26,TBMT01,TBMT02,TBMT09,TBMT14,TBMT26,TBMT32,TBMT36,TDDA69,TDDB44,TDDB68,TDDC17,TDDC34,TDDC73,TDDC78,TDDC90,TDDC93,TDDD04,TDDD07,TDDD08,TDDD14,TDDD17,TDDD20,TDDD23,TDDD25,TDDD27,TDDD29,TDDD37,TDDD38,TDDD41,TDDD43,TDDD48,TDDD49,TDDD50,TDDD53,TDDD55,TDDD56,TDDD57,TDDD60,TDDD62,TDDD69,TDDD70,TDDD75,TDDD79,TDDD84,TDDD86,TDDD88,TDDD89,TDDD91,TDDD94,TDDD95,TDDD96,TDDD97,TDDD98,TDDE01,TDDE02,TDDE05,TDDE06,TDDE07,TDDE09,TDDE13,TDDE14,TDDE15,TDDE16,TDDE19,TDDE20,TDDE21,TDDE23,TDDE24,TDDE25,TDDE30,TDDE31,TDDE34,TDDI07,TDEI13,TDEI19,TDEI35,TDP024,TDTS06,TDTS07,TDTS08,TDTS21,TEAE01,TEAE13,TEIE44,TEIE88,TEIM03,TEIM11,TEIO06,TEIO13,TEIO20,TEIO32,TEIO90,TFFM12,TFMT13,TFYA19,TFYA39,TFYA86,TFYA93,TGTU01,TGTU04,TGTU49,TGTU63,TGTU76,TGTU91,THEN18,THFR05,THSP05,THTY05,TKMJ15,TKMJ24,TNCG15,TNM048,TNM067,TNM079,TNM086,TNM095,TPTE06,TQxx33,TSBB06,TSBB08,TSBB09,TSBB11,TSBB15,TSBB17,TSBK02,TSBK03,TSBK07,TSBK08,TSDT14,TSDT84,TSEA22,TSEA26,TSEA29,TSEA44,TSEA82,TSEA83,TSEA84,TSEK02,TSEK03,TSEK06,TSEK11,TSEK37,TSEK38,TSFS02,TSFS04,TSFS06,TSFS09,TSIN01,TSIN02,TSIT02,TSIT03,TSKS01,TSKS05,TSKS10,TSKS11,TSKS12,TSKS13,TSKS14,TSKS15,TSRT04,TSRT07,TSRT08,TSRT09,TSRT10,TSRT12,TSRT14,TSRT62,TSRT78,TSTE06,TSTE08,TSTE12,TSTE14,TSTE17,TSTE24,TSTE25,TSTE26,TSTE85,TSTE86,TSTE87,TSTE93]


### Modeling selections
The selections are made on a slot basis, even though a course in fact runs across many slots. For instance TANA09. It has been solved by introducing a "occasion" data type which models one occasion the course is given.


### G1/G2 courses
There are many courses on G1/G2 level that are classified to semesters 1-6. The application should only differentiate on courses given on autumn or spring and let the user choose accordingly.

### Period 0
The course in discrete math appears in an imaginary "period 0" which causes it to end up in two blocks at the same time.

### Course list state
It seems like the list items in the course list carry their own state, so if item on index 1 is selected it will continue to be so even if the list should be filtered so that another course appears on that index. This was solved by introducint an internal state of the CourseList widget. However, this made it possible to select the same course by filtering the list. Apparently it is treated as a separate course if it has been filtered. To solve this the course list was rewritten to not use Reflex Workflow and instead simply rely on it's own state.

### Onmouseover firing
After rewriting the courselist to not use workflow (instead mapping the courses state to certain widgets) the onmouseover event fires continuously while mousing over an item. This has made me very upset.
It was solved by using foldDynMaybe to map the mouseevents onto Nothing and supress them firing alltogether.

### Rounded credits or not
The credits for a course should not be written out with decimals unless they are non-zero

### The state of widgets
Widgets should probably return an data types of themselves with fields of dynamics/events/behaviours instead of doing the Widget + WidgetEvent with pattern matching that's currently in place.

### Empty examinations
The examinations was parsed by scraping a "tbody" element while they appeared in a "table" element. Wonder if it has always been like this? Seems like the tests to not correspond to the markup being pulled from the URL.

### It seems like TATA42 is not being selected when clicked. Other courses with the same amount of slots seem to work though.

### CourseFields
Courses can in theory (and a few actually does) belong to more than one field and should be parsed accordingly. This has been done, which resulted in a new bug where course names with ',' in them get broken.

### FromHttpApiData and toHttpApiData
Apparently it is not possible to generate generic instances for FromHttpApiData and toHttpApiData, which is needed when saving a schedule. This has to be written by hand, and could very well be made efficiently using megaparsec, but for now a easy but excruciatingly slow implemnetation will have to do.


### Api route inference
The type hackery `client courseApi (Proxy :: Proxy m) (Proxy :: Proxy ()) url` being done to infer the api routes while parameterising on the url has lead to a lot of code duplication in `MasterExplorer.Client.Api`. There's got to be a better way of infering the types _once_ and reusing them.


### Saving and loading schedule
Currently the schedule is snapshotted upon save, so if a course in the schedule should be updated it will not be reflected if the user loads an old schedule.