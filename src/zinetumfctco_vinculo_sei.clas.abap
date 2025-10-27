class ZINETUMFCTCO_VINCULO_SEI definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods REGISTAR_VINCULO
    importing
      !INPUT type ZINETUMFCTVINCULO_SEI_REGISTA1
    exporting
      !OUTPUT type ZINETUMFCTVINCULO_SEI_REGISTAR
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_VINCULO_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZINETUMFCTCO_VINCULO_SEI IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINETUMFCTCO_VINCULO_SEI'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method REGISTAR_VINCULO.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'REGISTAR_VINCULO'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
