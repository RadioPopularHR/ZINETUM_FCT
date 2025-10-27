class ZINETUMFCTCO_ALTERAR_CONTRATO1 definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods ALTERAR_CONTRATO_TRABALHO
    importing
      !INPUT type ZINETUMFCTALTERAR_CONTRATO_TR4
    exporting
      !OUTPUT type ZINETUMFCTALTERAR_CONTRATO_TR3
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_ALTERAR_CONTRATO .
  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZINETUMFCTCO_ALTERAR_CONTRATO1 IMPLEMENTATION.


  method ALTERAR_CONTRATO_TRABALHO.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'ALTERAR_CONTRATO_TRABALHO'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINETUMFCTCO_ALTERAR_CONTRATO1'
    logical_port_name   = logical_port_name
  ).

  endmethod.
ENDCLASS.
