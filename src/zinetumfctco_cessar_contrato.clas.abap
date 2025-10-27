class ZINETUMFCTCO_CESSAR_CONTRATO definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CESSAR_CONTRATO
    importing
      !INPUT type ZINETUMFCTCESSAR_CONTRATO1
    exporting
      !OUTPUT type ZINETUMFCTCESSAR_CONTRATO_RES1
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_GFCTEXCEPTION .
  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZINETUMFCTCO_CESSAR_CONTRATO IMPLEMENTATION.


  method CESSAR_CONTRATO.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'CESSAR_CONTRATO'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINETUMFCTCO_CESSAR_CONTRATO'
    logical_port_name   = logical_port_name
  ).

  endmethod.
ENDCLASS.
