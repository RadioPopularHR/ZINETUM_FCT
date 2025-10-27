class ZINETUMFCTCO_ALTERAR_CONTRATO definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods CRIAR_PERIODO_RENDIMENTO
    importing
      !INPUT type ZINETUMFCTCRIAR_PERIODO_RENDI2
    exporting
      !OUTPUT type ZINETUMFCTCRIAR_PERIODO_RENDI1
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_GFCTEXCEPTION .
  methods REMOVER_PERIODO_RENDIMENTO
    importing
      !INPUT type ZINETUMFCTREMOVER_PERIODO_REN2
    exporting
      !OUTPUT type ZINETUMFCTREMOVER_PERIODO_REN1
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_GFCTEXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZINETUMFCTCO_ALTERAR_CONTRATO IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINETUMFCTCO_ALTERAR_CONTRATO'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method CRIAR_PERIODO_RENDIMENTO.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'CRIAR_PERIODO_RENDIMENTO'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method REMOVER_PERIODO_RENDIMENTO.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'REMOVER_PERIODO_RENDIMENTO'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
