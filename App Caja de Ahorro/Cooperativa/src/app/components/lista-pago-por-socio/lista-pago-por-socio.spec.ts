import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListaPagoPorSocio } from './lista-pago-por-socio';

describe('ListaPagoPorSocio', () => {
  let component: ListaPagoPorSocio;
  let fixture: ComponentFixture<ListaPagoPorSocio>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListaPagoPorSocio]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ListaPagoPorSocio);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
