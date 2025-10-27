class ZINETUMFCTCO_REGISTAR_PERIODO definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods REGISTAR_PERIODO_RENDIMENTO
    importing
      !INPUT type ZINETUMFCTREGISTAR_PERIODO_RE4
    exporting
      !OUTPUT type ZINETUMFCTREGISTAR_PERIODO_RE3
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_REGISTAR_PERIODO .
protected section.
private section.
ENDCLASS.



CLASS ZINETUMFCTCO_REGISTAR_PERIODO IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINETUMFCTCO_REGISTAR_PERIODO'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method REGISTAR_PERIODO_RENDIMENTO.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'REGISTAR_PERIODO_RENDIMENTO'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
