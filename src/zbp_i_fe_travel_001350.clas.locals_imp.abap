CLASS LHC_TRAVEL DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.

    METHODS:
      CALCULATETRAVELID FOR DETERMINE ON SAVE
        IMPORTING keys FOR  Travel~CalculateTravelID ,
      ShowTestMessage FOR MODIFY
        IMPORTING keys FOR ACTION Travel~ShowTestMessage.

    METHODS ValidateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~ValidateAgency.

    METHODS ValidateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~ValidateCustomer.

    METHODS ValidateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~ValidateDates.
    METHODS DetermineStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~DetermineStatus.

    METHODS DetermineTravelId FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~DetermineTravelId.

    METHODS ValidateCurrencyCode FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~ValidateCurrencyCode.

    METHODS CancelTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~CancelTravel RESULT result.

ENDCLASS.

CLASS LHC_TRAVEL IMPLEMENTATION.
  METHOD CALCULATETRAVELID.
  ENDMETHOD.

  METHOD showtestmessage.
    DATA message TYPE REF TO zcm_travel_001350.

    message = NEW zcm_travel_001350( severity  = if_abap_behv_message=>severity-success
                              textid    = zcm_travel=>test_message
                              user_name = sy-uname ).

    APPEND message TO reported-%other.
  ENDMETHOD.

  METHOD ValidateCurrencyCode.
    DATA message TYPE REF TO zcm_travel_001350.

    " Read travels
    READ ENTITY IN LOCAL MODE ZI_FE_TRAvel_001350
      FIELDS ( CurrencyCode )
      WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

  ENDMETHOD.

  METHOD validateagency.
    DATA message TYPE REF TO zcm_travel_001350.

    " Read Travels
    READ ENTITY IN LOCAL MODE ZI_FE_Travel_001350
         FIELDS ( AgencyId )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    " Process Travels
    LOOP AT travels INTO DATA(travel).
      " Validate Agency and Create Error Message
      SELECT SINGLE FROM /dmo/agency FIELDS @abap_true WHERE agency_id = @travel-AgencyId INTO @DATA(exists).
      IF exists = abap_false.
        message = NEW zcm_travel_001350( textid    = zcm_travel_001350=>no_agency_found
                                  agency_id = travel-AgencyId ).
        APPEND VALUE #( %tky     = travel-%tky
                        %element = VALUE #( AgencyId = if_abap_behv=>mk-on )
                        %msg     = message ) TO reported-travel.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validatecustomer.
    DATA message TYPE REF TO zcm_travel_001350.

    " Read Travels
    READ ENTITY IN LOCAL MODE ZI_FE_Travel_001350
         FIELDS ( CustomerId )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    " Process Travels
    LOOP AT travels INTO DATA(travel).
      " Validate Agency and Create Error Message
      SELECT SINGLE FROM /dmo/customer FIELDS @abap_true WHERE customer_id = @travel-CustomerId INTO @DATA(exists).
      IF exists = abap_false.
        message = NEW zcm_travel_001350( textid      = zcm_travel_001350=>no_customer_found
                                  customer_id = travel-CustomerId ).
        APPEND VALUE #( %tky     = travel-%tky
                        %element = VALUE #( CustomerId = if_abap_behv=>mk-on )
                        %msg     = message ) TO reported-travel.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validatedates.
    DATA message TYPE REF TO zcm_travel_001350.

    " Read Travels
    READ ENTITY IN LOCAL MODE ZI_FE_Travel_001350
         FIELDS ( BeginDate EndDate )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    " Process Travels
    LOOP AT travels INTO DATA(travel).
      " Validate Dates and Create Error Message
      IF travel-EndDate < travel-BeginDate.
        message = NEW zcm_travel_001350( textid = zcm_travel_001350=>invalid_dates ).
        APPEND VALUE #( %tky = travel-%tky
                        %msg = message ) TO reported-travel.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.



  METHOD DetermineStatus.

  " Read Travels
    READ ENTITY IN LOCAL MODE ZI_FE_Travel_001350
         FIELDS ( OverallStatus )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    " Modify Travels
    MODIFY ENTITY IN LOCAL MODE ZI_FE_Travel_001350
           UPDATE FIELDS ( OverallStatus )
           WITH VALUE #( FOR t IN travels
                         ( %tky   = t-%tky
                           OverallStatus = 'O' ) ).

  ENDMETHOD.

  METHOD DetermineTravelId.

  " Read Travels
    READ ENTITY IN LOCAL MODE ZI_FE_Travel_001350
         FIELDS ( TravelId )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    " Process Travels
    LOOP AT travels REFERENCE INTO DATA(travel).
      " Get Max Travel ID
      SELECT FROM /dmo/travel FIELDS MAX( travel_id ) INTO @DATA(max_travel_id).

      " Set Travel ID
      travel->TravelId = max_travel_id + 1.
    ENDLOOP.

    " Modify Travels
    MODIFY ENTITY IN LOCAL MODE ZI_FE_Travel_001350
           UPDATE FIELDS ( TravelId )
           WITH VALUE #( FOR t IN travels
                         ( %tky     = t-%tky
                           TravelId = t-TravelId ) ).

  ENDMETHOD.

  METHOD CancelTravel.
*   DATA message TYPE REF TO zcm_travel_001350.
*
*    " Read Travels
*    READ ENTITY IN LOCAL MODE ZI_FE_Travel_001350
*         ALL FIELDS
*         WITH CORRESPONDING #( keys )
*         RESULT DATA(travels).
*
*    " Process Travels
*    LOOP AT travels REFERENCE INTO DATA(travel).
*      " Validate Status and Create Error Message
*      IF travel->OverallStatus = 'X'.
*        message = NEW zcm_travel_001350( textid = zcm_travel_001350=>travel_already_cancelled
*                        travel = travel->Description ).
*        APPEND VALUE #( %tky     = travel->%tky
*                        %element = VALUE #( OverallStatus = if_abap_behv=>mk-on )
*                        %msg     = message ) TO reported-travel.
*        APPEND VALUE #( %tky = travel->%tky ) TO failed-travel.
*        DELETE travels INDEX sy-tabix.
*        CONTINUE.
*      ENDIF.
*
*      " Set Status to Cancelled and Create Success Message
*      travel->OverallStatus = 'X'.
*      message = NEW zcm_travel_001350( severity = if_abap_behv_message=>severity-success
*                                textid   = zcm_travel_001350=>travel_cancelled_successfully
*                                travel   = travel->Description ).
*      APPEND VALUE #( %tky     = travel->%tky
*                      %element = VALUE #( OverallStatus = if_abap_behv=>mk-on )
*                      %msg     = message ) TO reported-travel.
*    ENDLOOP.
*
*    " Modify Travels
*    MODIFY ENTITY IN LOCAL MODE ZI_FE_Travel_001350
*           UPDATE FIELDS ( OverallStatus )
*           WITH VALUE #( FOR t IN travels
*                         ( %tky = t-%tky OverallStatus = t-OverallStatus ) ).
*
*    " Set Result
*    result = VALUE #( FOR t IN travels
*                      ( %tky   = t-%tky
*                        %param = t ) ).
  ENDMETHOD.

ENDCLASS.
