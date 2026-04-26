import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListaPrestamoPorSocio } from './lista-prestamo-por-socio';

describe('ListaPrestamoPorSocio', () => {
  let component: ListaPrestamoPorSocio;
  let fixture: ComponentFixture<ListaPrestamoPorSocio>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListaPrestamoPorSocio]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ListaPrestamoPorSocio);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
