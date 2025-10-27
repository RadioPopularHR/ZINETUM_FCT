class ZINETUMFCTCO_CONTRATO_SEI definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods GET_DADOS_CONTRATOS
    importing
      !INPUT type ZINETUMFCTCONTRATO_SEI_GET_DA1
    exporting
      !OUTPUT type ZINETUMFCTCONTRATO_SEI_GET_DAD
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_CONTRATO_EXCEPTIO .
  methods PESQUISA_CONTRATOS
    importing
      !INPUT type ZINETUMFCTCONTRATO_SEI_PESQUI1
    exporting
      !OUTPUT type ZINETUMFCTCONTRATO_SEI_PESQUIS
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_CONTRATO_EXCEPTIO .
protected section.
private section.
ENDCLASS.



CLASS ZINETUMFCTCO_CONTRATO_SEI IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINETUMFCTCO_CONTRATO_SEI'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method GET_DADOS_CONTRATOS.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'GET_DADOS_CONTRATOS'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method PESQUISA_CONTRATOS.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'PESQUISA_CONTRATOS'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
