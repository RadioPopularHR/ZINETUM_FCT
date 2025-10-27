class ZINETUMFCTCO_CESSAR_VINCULO_SE definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CESSAR_VINCULO
    importing
      !INPUT type ZINETUMFCTCESSAR_VINCULO_SEI_1
    exporting
      !OUTPUT type ZINETUMFCTCESSAR_VINCULO_SEI_C
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_CESSAR_VINCULO_EX .
  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZINETUMFCTCO_CESSAR_VINCULO_SE IMPLEMENTATION.


  method CESSAR_VINCULO.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'CESSAR_VINCULO'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINETUMFCTCO_CESSAR_VINCULO_SE'
    logical_port_name   = logical_port_name
  ).

  endmethod.
ENDCLASS.
