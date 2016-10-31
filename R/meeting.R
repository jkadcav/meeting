library(jsonlite)

em <- new.env()
em_meetings<-''
em_meeting_date<-''
em_meeting_animal<-''

#' Retrieve meeting events
#'
#' @param meetingId meeting ID to use for retrival
#' @keywords events
#' @export
#' @examples
#' retrieve_meetings()
retrieve_meetings<-function( date, animal ) {
  em_meetings <- get("em_meetings", envir = em)
  em_meeting_date <- get("em_meeting_date", envir = em)
  em_meeting_animal <- get("em_meeting_animal", envir = em)

  if( date==em_meeting_date && animal==em_meeting_animal ) {
    meetings<-em_meetings
  } else {
    api_response<-jsonlite::fromJSON(paste('http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/meetings?filters%5Bmeeting_date%5D=',date,'&filters%5Bvenue_types%5D%5B%5D=',animal,sep=""))
    meetings<-eval(parse(text=paste('api_response$data$meetings')))
    assign("em_meetings", meetings, envir = em)
    assign("em_meeting_date", date, envir = em)
    assign("em_meeting_animal", animal, envir = em)
  }

  return(meetings)
}

#' Extract properties from an object
#'
#' @param obj Object to traverse
#' @param arr Array of property values to look at e.g. meetings$12029, meetings$12030
#' @param propertyName Property to extract from each traversed object
#' @keywords json, utils
#' @export
#' @examples
#' extract_property( all_meetings, meetingIds, 'name' )
#' extract_property( { '12345': { 'name': 'x' } }, [ 12345 ], 'name' ) > [ x ]
extract_property<-function( obj, arr, propName ) {
  apply_it<-function( item_value ){
    a<-paste('obj$`',item_value,'`$',propName,sep="")
    b<-eval(parse(text=a))
    return(b)
  }

  value_list<-mapply(apply_it, arr)

  return(value_list)
}

#' Retrieve meeting ID
#'
#' This function allows you to express your love of cats.
#' @param date date of meeting
#' @param animal venue type
#' @param course venue name
#' @keywords meeting
#' @export
#' @examples
#' retrieveMeeting()
retrieve_meeting_id<-function(date,animal,course){
  all_meetings<-retrieve_meetings(date,animal)
  t.meets<-length(all_meetings)
  meets = names(all_meetings)

  courses<-extract_property( all_meetings, meets, 'name' )

  meetings<-data.frame(matrix(NA,t.meets,3))
  colnames(meetings)<-c('Date','Course','MeetingID')
  meetings$Date<-date
  meetings$Course<-toupper(courses)
  meetings$MeetingID<-meets
  id<-as.numeric(meetings$MeetingID[meetings$Course==course])
  return(id)
}

retrieve_course<-function(date,animal,meetingid){
  all_meetings<-retrieve_meetings(date,animal)
  #events<-fromJSON(paste('http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/meetings?filters%5Bmeeting_date%5D=',date,'&filters%5Bvenue_types%5D%5B%5D=',animal,sep=""))
  b<-paste('all_meetings$`',meetingid,'`$name',sep="")
  course<-eval(parse(text=b))
  return(course)
}
